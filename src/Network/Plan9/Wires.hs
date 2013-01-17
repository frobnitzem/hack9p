{-# LANGUAGE BangPatterns #-}
module Network.Plan9.Wires where

import qualified Control.Wire.TimedMap as Tm
import Control.Arrow
import qualified Data.Map as M
import Data.Maybe (fromMaybe,mapMaybe)
import qualified Data.Traversable as T
import qualified Data.Set as S
import Control.Monad (foldM)
{- Note that Netwire 4.0.7 doesn't export TimedMap's constructor, 
 - so I altered it as follows:
 -           TimedMap(..),
 -}
import Control.Wire.TimedMap ( TimedMap (..) )
import Control.Wire.Wire
import Control.Wire.Prefab (time)
import Data.Map (Map)


-- | A StrategyWire is a humble worker in a sea of IO.
type StrategyWire m a b = Wire b m (Maybe a) (b,Time)

-- | A WireMap contains a used-keyed list of wires and their
-- corresponding review appointment times.  By virtue of the TimedMap
-- it is able to split the list by appointment time.
type WireMap k m a b = TimedMap Time k (StrategyWire m a b)

-- | This function returns a 'fork' wire, which dynamically
-- builds a set of handlers (Wires) based on keyed values.
--
-- Each forked process has the type 'StrategyWire'.
-- The input is a Maybe, since this wire must implement
-- two call-backs, a 'new input' callback on (Just a)
-- and a 'heartbeat' callback on Nothing.
--
-- The process also signals when it's done by inhibiting
-- with a final return value.  If it wants to re-run,
-- it instead outputs a return value and a time extension,
-- signifying the minimum amount of time from the current
-- run to wait before issuing another 'heartbeat' callback.
--
-- This design means that all signals, Maybes, etc., have to be
-- hidden in the return value, 'b'.  The case where the wire
-- continues on but does not ``produce'' a value is handled
-- by the user definining b = Maybe b', then running
-- mapMaybe on the result of the fork.
--
-- Multiple wires may be tried at any instant,
-- so the return is a list, with the first element
-- always formed from the key,value pair input.
-- If the wire succeeds, the list always has at least that
-- first element.
--
-- There are no run delays, so it's possible for a wire
-- to be created, run, and removed in a single
-- invocation of the fork. (the usual use case)
--
-- Usage:
-- The first argument is a wire which takes a key
-- and returns a wire to be added to the map and
-- run immediately with input 'Just a'.
--
-- Note: Incorrect usage can lead to a memory leak.  To avoid this,
-- ensure that your wires will inhibit at some point, possibly
-- through an internal 'total timer' mechanism.
--
-- * Complexity:  O(n) space, O(log n) time wrt to number of stored
-- contexts, O(n) in time, since all wires may insist on always being run.
--
-- * Depends: current instant.
--
-- * Inhibits: when a new wire needs to be created, but
-- the wire creator decides to inhibit instead.
fork :: (Monad m, Ord k)
     => Wire e m k (StrategyWire m a b) 
     -> Wire (e,[(k,b)]) m (k,a) [(k,b)]
fork genr = loopArr Tm.empty $ {- ((k,a), WireMap) -}
	second ((time &&& arr id) >>> cleaner) {- ((k, a), ([(k,b)], (Time,WireMap))) -}
	  >>> arr (\ (ka, (lb, twm)) -> ((ka,twm), lb)) {- (((k, a), (Time,WireMap)), [(k, b)]) -}
	  >>> (first (runFork genr {- ((k, b), (Time, WireMap)) -}) -- Route via WireMap
	         >>> arr (\ ((kb,(t,wm)),lkb) -> (kb:lkb, wm)) {- ([(k, b)], WireMap) -}
	       <?> mkFix (\_ (((_, (_,wm)), lkb),e) -> Left ((e,lkb),wm)))

-- | I vant to vash and vipe your vindows.
cleaner :: (Monad m, Ord k) => Wire e m (Time, WireMap k m a b) ([(k,b)], (Time, WireMap k m a b))
cleaner = mkGen $ \ _ (t, wm) -> do
	    let run1 :: (Monad m, Ord k)
		    => ([(k,b)], WireMap k m a b)
		    -> (k, (StrategyWire m a b, Time))
		    -> m ([(k,b)], WireMap k m a b)
	        run1 (lkb, wm') (k, (w0, t0)) = do
		    let dt = t - t0
		    (eb, w) <- dt `seq` stepWire w0 dt Nothing
		    return $ case eb of
			Right (b,etn) -> ((k,b):lkb, Tm.insert (t+etn) k w wm')
			Left  b     -> ((k,b):lkb, Tm.delete k wm')
	    (lkb, rwm) <- foldM run1 ([],wm) (splitAssoc t wm)
	    return (Right (lkb, (t,rwm)), cleaner)

-- | The insertion time (t0) is the absolute expiration time.
-- dt < 0 => unexpired, >= 0 => expired.
runFork :: (Monad m, Ord k)
	=> Wire e m k (StrategyWire m a b)
	-> Wire e m ((k,a), (Time, WireMap k m a b))
		    ((k,b), (Time, WireMap k m a b))
runFork genr' = mkGen $ \ dt ((k,a), (t,wm')) -> do
	    let {-run1 :: (Monad m, Ord k)
                    => (StrategyWire m a b, Time)
                    -> m (b, Maybe(WireMap k m a b -> WireMap k m a b)) -}
		run1 (w0,t0) = do
		    let dt = t - t0
		    (eb, w) <- dt `seq` stepWire w0 dt (Just a)
		    return $ case eb of
			Right (b,etn) -> (b, Just (Tm.insert (t+etn) k w))
			Left  b     -> (b, Nothing)
	    
	    case Tm.lookup k wm' of
		Just wt0 -> do
			(b, mtrans) <- run1 wt0
			let wm = fromMaybe (Tm.delete k) mtrans wm' -- delete on inh
			return (Right ((k,b), (t,wm)), runFork genr')
		Nothing -> do
			(ew, genr) <- stepWire genr' dt k
			case ew of
			    Left err -> return (Left err, runFork genr)
			    Right w -> do
				(b, mtrans) <- run1 (w, t)
				let wm = fromMaybe id mtrans wm' -- ignore on inh
				return (Right ((k,b), (t,wm)), runFork genr)
	    --wm `seq` return (Right (b, wm), runStMap genr) -- require eval. of wm??

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
splitAssoc :: (Ord k, Ord t) => t -> TimedMap t k a -> [(k,(a,t))]
splitAssoc t0 (TimedMap mk' mt') = mk
    where
    (older', middle, mt) = M.splitLookup t0 mt'
    mk =
	mapMaybe (\ k -> fmap (\v->(k,v)) $ M.lookup k mk') .
        S.toList .
        M.foldl' S.union S.empty .
        maybe id (M.insert t0) middle $ older'

-- | Handle an inhibiting wire by either throwing another
-- inhibition value or producing an output value.
--
-- * Inhibits: when the first wire inhibits and the second
-- argument returns 'Left err'

handler :: (Monad m) => Wire e m a b -> Wire e' m (a,e) b -> Wire e' m a b
handler w' h' = handler' 0 w' h'
    where handler' !t w0 h0 = mkGen $ \dt a -> do
                (eb, w) <- stepWire w0 dt a
                case eb of
                    (Left  e) -> do
                        (eb', h) <- stepWire h0 (t+dt) (a,e)
                        return (eb', handler w h)
                    (Right b) -> return (Right b, handler' (t+dt) w h0)
infix 0 <?>

-- | Synonym for `handler`

(<?>) :: (Monad m) => Wire e m a b -> Wire e' m (a,e) b -> Wire e' m a b
(<?>) = handler

