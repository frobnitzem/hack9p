{-# LANGUAGE BangPatterns #-}
module Network.Plan9.Wires where

import qualified Control.Wire.TimedMap as Tm
import Control.Arrow
import qualified Data.Map as M
import Data.Maybe (fromMaybe,mapMaybe)
import qualified Data.Traversable as T
import qualified Data.Set as S
import Control.Monad (foldM)
{- Note that Netwire 4.0.5 doesn't export TimedMap's constructor, 
 - so I altered it as follows:
 -           TimedMap(..),
 -}
import Control.Wire.TimedMap ( TimedMap (..) )
import Control.Wire.Wire
import Control.Wire.Prefab (time)
import Data.Map (Map)

-- | This function returns a "DynamicSet" wire.  It never inhibits, but
-- maps the input (a) over all the a->b wires in the set.
-- It shares some similarities with Netwire.Trans.Combine.multicast, except
-- for the dynamic addition/removal of wires.
-- Wires are removed from the set when they inhibit.
--
-- Usage:
-- The first argument is a function that creates new wires (from the
--   second input to the DynamicSet)
-- The second argument is an initial list of wires in the set.
-- Originally from:
-- http://jshaskell.blogspot.com/2012/11/breakout-improved-and-with-netwire.html
dynamicSet :: (Monad m) => (c -> Wire e m a b) -> [Wire e m a b] ->  Wire e m (a, [c]) [b]
dynamicSet creator ws' = mkGen $ \dt (i,new) -> do
            res <- mapM (\w -> stepWire w dt i) ws'
            let filt (Right a, b) = Just (a,b)
                filt _            = Nothing
                resx = mapMaybe filt res
            return (Right $ (fmap fst resx), dynamicSet creator $ (fmap snd resx) ++ (map creator new))

-- | This function returns a "DynamicMap" wire.  It combines parts of
-- the context wire from Netwire.Trans.Combine (which dynamically
-- creates wires) with a dynamicSet wire, where new wires
-- added to the map can signal their removal by inhibiting.
-- Multiple wires may be tried at any instant,
-- so the return is a list.
--
-- The created wire may output with a type (b, Time), to indicate
-- that it should be re-run later and optionally return something
-- or may inhibit with the type b to indicate that it should
-- output b and be immediately removed from the Map.
--
-- The case where the wire continues on but does not produce a value is handled
-- by the user definining b = Maybe b', then running mapMaybe on the result
-- of the DynamicMap.
--
-- Because the context wire may have be in a timed re-run,
-- its input type is a Maybe a.
--
-- There are no run delays, so it's possible for a wire
-- to be created, run, and removed in a single
-- invocation of the DynamicMap. (the usual use case)
--
-- Usage:
-- The first argument is a function which takes a key
-- and returns a wire to be added to the map and
-- run immediately with input 'Just a'.
--
-- Note: Incorrect usage can lead to a memory leak.  To avoid this,
-- ensure that your wires will inhibit with val. Nothing at some
-- point, possibly through a timer mechanism.
--
-- * Complexity:  O(n) space, O(log n) time wrt to number of stored
-- contexts.
--
-- * Depends: current instant.
--
-- * Inhibits: never
type StrategyWire m a b = Wire b m (Maybe a) (b,Time)
dynamicMap' ::
    {-forall a b m k.-} (Monad m, Ord k)
    => (k -> StrategyWire m a b)
    -> Wire () m (k,a) [b] -- 'k' are the key values
dynamicMap' genr = dynah Tm.empty 0
    where
    --dynah :: (Monad m, Ord k) => TimedMap Time k (StrategyWire m a b)
--			-> Time -> Wire () m (k,a) [b]
    dynah !ctxs' t' =
        mkGen $ \dt' (k,a) -> do
            let t        = t' + dt' -- Remember William, the clock in
				    --   San Dimas is always ticking.
	    (lb, trans)  <- hassle t (splitTm t ctxs') -- old business
	    let ctxs = trans ctxs'
	        (wt0, rem) = case Tm.lookup k ctxs of
			Just v -> (v, Tm.delete k) -- delete on inh
			Nothing -> ((genr k, t), id) -- ignore on inh
	    (b, mtrans) <- run1 k t wt0 (Just a) -- new business
	    return (Right (b : lb), dynah ((fromMaybe rem mtrans) ctxs) t)
  {-  run1 :: (Monad m, Ord k)
	=> k
	-> Time
	-> (StrategyWire m a b, Time)
	-> Maybe a
	-> m (b, Maybe (TimedMap Time k (StrategyWire m a b)
                        -> TimedMap Time k (StrategyWire m a b))) -}
    run1 k t (w',t0) ma = do -- The insertion time (t0) is the absolute expiration time.
	    let dt = t - t0 -- dt <= 0 => unexpired, > 0 => expired.
            (eb, w) <- dt `seq` stepWire w' dt ma
            return $ case eb of
		Right (b,etn) -> (b, Just (Tm.insert (t+etn) k w))
		Left  b     -> (b, Nothing)
    -- Hassle all wires past deadline (pref. in most to least offending order)
 {- hassle :: (Monad m, Ord k)
	      => Time
	      -> [(StrategyWire m a b, Time)]
	      -> m ([b],   TimedMap Time k (StrategyWire m a b)
			-> TimedMap Time k (StrategyWire m a b)) -}
    hassle t = foldM run1' ([],id)
	where run1' :: (Monad m, Ord k)
		    => ([b], TimedMap Time k (StrategyWire m a b)
                          -> TimedMap Time k (StrategyWire m a b))
		    -> (k,(StrategyWire m a b, Time))
		    -> m ([b], TimedMap Time k (StrategyWire m a b)
                            -> TimedMap Time k (StrategyWire m a b))
	      run1' (lb',trans') (k,wt0) = do
		(b, mtrans) <- run1 k t wt0 Nothing
	        let rem = Tm.delete k
		    trans = fromMaybe rem mtrans
	        return (b : lb', trans . trans')


 {- hassle :: (Monad m, Ord k)
	      => k
	      -> Time
	      -> [(StrategyWire m a b, Time)]
	      -> m ([b],   TimedMap Time k (StrategyWire m a b)
			-> TimedMap Time k (StrategyWire m a b)) -}

type WireMap k m a b = TimedMap Time k (StrategyWire m a b)

cleaner :: (Monad m, Ord k) => Wire e m (Time, WireMap k m a b) ([b], WireMap k m a b)
cleaner = mkGen $ \ _ (t, wm) -> do
	    let run1 :: (Monad m, Ord k)
		    => ([b], WireMap k m a b)
		    -> (k, (StrategyWire m a b, Time))
		    -> m ([b], WireMap k m a b)
	        run1 (lb, wm') (k, (w0, t0)) = do
		    let dt = t - t0
		    (eb, w) <- dt `seq` stepWire w0 dt Nothing
		    return $ case eb of
			Right (b,etn) -> (b:lb, Tm.insert (t+etn) k w wm')
			Left  b     -> (b:lb, Tm.delete k wm')
	    (lb, rwm) <- foldM run1 ([],wm) (splitTm t wm)
	    return (Right (lb, rwm), cleaner)

-- The insertion time (t0) is the absolute expiration time.
-- dt < 0 => unexpired, >= 0 => expired.
runStMap :: (Monad m, Ord k)
	=> (k -> StrategyWire m a b)
	-> Wire e m ((k,a), (Time, WireMap k m a b)) (b, WireMap k m a b)
runStMap genr = mkGen $ \ _ ((k,a), (t,wm')) -> do
	    let {-run1 :: (Monad m, Ord k)
                    => (StrategyWire m a b, Time)
                    -> m (b, Maybe(WireMap k m a b -> WireMap k m a b)) -}
		run1 (w0,t0) = do
		    let dt = t - t0
		    (eb, w) <- dt `seq` stepWire w0 dt (Just a)
		    return $ case eb of
			Right (b,etn) -> (b, Just (Tm.insert (t+etn) k w))
			Left  b     -> (b, Nothing)
	    let (wt0, rem) = case Tm.lookup k wm' of
			Just v -> (v, Tm.delete k) -- delete on inh
			Nothing -> ((genr k, t), id) -- ignore on inh
	    (b, mtrans) <- run1 wt0
	    let wm = fromMaybe rem mtrans wm'
	    return (Right (b, wm), runStMap genr)
	    --wm `seq` return (Right (b, wm), runStMap genr) -- require eval. of wm??

-- A DynamicMap is a set of wires:
dynamicMap genr = loopArr Tm.empty $ {- ((k,a), WireMap) -}
	(arr id &&& time) {- (((k, a), WireMap), Time) -}
	  >>> arr (\ ((x,y), t) -> ((t,x), (t,y))) {- ((Time, (k, a)), (Time, WireMap)) -}
	  >>> second (cleaner {- ([b], WireMap) -})
	  >>> arr (\ ((t,ka), (lb, wm)) -> ((ka,(t,wm)), lb)) {- (((k, a), (Time,WireMap)), [b]) -}
	  >>> first (runStMap genr {- (b, WireMap) -}) -- Route via WireMap
	  >>> arr (\ ((b,wm),lb) -> (b:lb, wm)) {- ([b], WireMap) -}

-- | Construct a wire from the given local state transision
-- wire.  The transition wire always produces a new state, even if
-- it inhibits.
loopArr ::
    (Monad m)
    => s
    -> Wire (e,s) m (a,s) (b,s)
    -> Wire e m a b
loopArr s0 w0 = mkGen $ \dt a -> do
		    (mbs, w) <- stepWire w0 dt (a, s0)
		    return $ case mbs of
			Right (b, s) -> (Right b, loopArr s w)
			Left  (e, s) -> (Left  e, loopArr s w)

-- | Return an association list of (k,(a,t)) values with t <= t0.
-- This really belongs to Control.Wire.TimedMap
splitTm :: (Ord k, Ord t) => t -> TimedMap t k a -> [(k,(a,t))]
splitTm t0 (TimedMap mk' mt') = mk
    where
    (older', middle, mt) = M.splitLookup t0 mt'
    mk =
	mapMaybe (\ k -> fmap (\v->(k,v)) $ M.lookup k mk') .
        S.toList .
        M.foldl' S.union S.empty .
        maybe id (M.insert t0) middle $ older'

