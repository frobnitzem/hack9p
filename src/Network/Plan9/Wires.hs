{-# LANGUAGE BangPatterns #-}
module Network.Plan9.Wires where

import qualified Control.Wire.TimedMap as Tm
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
dynamicMap ::
    forall a b m k. (Monad m, Ord k)
    => (k -> StrategyWire m a b)
    -> Wire () m (k,a) [b] -- 'k' are the key values
dynamicMap gen = dynah Tm.empty 0
    where
    dynah :: TimedMap Time k (StrategyWire m a b)
			-> Time -> Wire () m (k,a) [b]
    dynah !ctxs' t' =
        mkGen $ \dt' (k,a) -> do
            let t        = t' + dt' -- Remember William, the clock in
				    --   San Dimas is always ticking.
	    (lb, trans)  <- hassle k t (splitTm t ctxs') -- old business
	    let ctxs = trans ctxs'
	        (wt0, rem) = case Tm.lookup k ctxs of
			Just v -> (v, Tm.delete k) -- delete on inh
			Nothing -> ((gen k, t), id) -- ignore on inh
	    (b, mtrans) <- run1 k t wt0 (Just a) -- new business
	    return (Right (b : lb), dynah ((fromMaybe rem mtrans) ctxs) t)
    run1 :: k
	-> Time
	-> (StrategyWire m a b, Time)
	-> Maybe a
	-> m (b, Maybe (TimedMap Time k (StrategyWire m a b)
                        -> TimedMap Time k (StrategyWire m a b)))
    run1 k t (w',t0) ma = do -- The insertion time (t0) is the absolute expiration time.
	    let dt = t - t0 -- dt <= 0 => unexpired, > 0 => expired.
            (eb, w) <- dt `seq` stepWire w' dt ma
            return $ case eb of
		Right (b,etn) -> (b, Just (Tm.insert (t+etn) k w))
		Left  b     -> (b, Nothing)
    -- Hassle all wires past deadline (pref. in most to least offending order)
    hassle ::    k
	      -> Time
	      -> [(StrategyWire m a b, Time)]
	      -> m ([b],   TimedMap Time k (StrategyWire m a b)
			-> TimedMap Time k (StrategyWire m a b))
    hassle k t = foldM run1' ([],id)
	where run1' :: ([b], TimedMap Time k (StrategyWire m a b)
                          -> TimedMap Time k (StrategyWire m a b))
		    -> (StrategyWire m a b, Time)
		    -> m ([b], TimedMap Time k (StrategyWire m a b)
                            -> TimedMap Time k (StrategyWire m a b))
	      run1' (lb',trans') wt0 = do
		(b, mtrans) <- run1 k t wt0 Nothing
	        let rem = Tm.delete k
		    trans = fromMaybe rem mtrans
	        return (b : lb', trans . trans')

-- | Return a list of (a,t) values with t <= t0
splitTm :: (Ord k, Ord t) => t -> TimedMap t k a -> [(a,t)]
splitTm t0 (TimedMap mk' mt') = mk
    where
    (older', middle, mt) = M.splitLookup t0 mt'
    mk =
	mapMaybe (\ k -> M.lookup k mk') .
        S.toList .
        M.foldl' S.union S.empty .
        maybe id (M.insert t0) middle $ older'

