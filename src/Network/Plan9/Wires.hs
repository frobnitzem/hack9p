{-# LANGUAGE BangPatterns, TupleSections #-}
module Network.Plan9.Wires where

import qualified Control.Wire.TimedMap as Tm
import Control.Monad.Identity (Identity)
import Control.Wire.TimedMap ( TimedMap (..) )
import Control.Arrow
import qualified Data.Map as M
import Data.Maybe (fromMaybe,mapMaybe)
--import qualified Data.Traversable as T
import qualified Data.Set as S
import Control.Monad (foldM)
import Data.Monoid (Monoid,mempty,mappend)
{- Note that Netwire 4.0.7 doesn't export TimedMap's constructor, 
 - so I altered it as follows:
 -           TimedMap(..),
 -}
import Control.Wire.Wire
import Control.Applicative
--import Control.Wire.Prefab (time,after)
import Control.Wire (time,after,id,(.),inhibit)
import Data.Map (Map)

import Prelude hiding ((.), id)

-- | The output of a case statement for routing incoming data
-- across wires stored in a structure.
type WCases st e m a b c = (a, st -> Wire e m a b, Wire e m a b -> st -> st, Wire e m b c)

-- | A CaseWire is a wire that keeps an internal struct of wires,
-- and routes to each depending upon some case-matching on the input.
-- It thus needs the matching function to produce the wire input,
-- the struct destructor / constructor, and a wire to process
-- the output.  Using these, it nicely wraps the case-computation.
-- TODO: Use the dt value by keeping a record of the time of last call
-- to each structure member...
caseWire :: (Monad m)
	 => (a -> WCases st e m a' b c)
	 -> st
	 -> Wire e m a c
caseWire cfn st = mkGen $ \ dt a -> do
	let (a', dst, cst, wbc) = cfn a
	    w = dst st
	(eb, w') <- stepWire w dt a'
	let self = caseWire cfn (cst w' st)
	case eb of
	    Left err -> return (Left err, self)
	    Right b -> do
		(ec, _) <- stepWire wbc dt b
		return (ec, self)


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
	second (cleaner 0) {- ((k, a), ([(k,b)], WireMap)) -}
	  >>> arr (\ (ka, (lb, wm)) -> ((ka,wm), lb)) {- (((k, a), WireMap), [(k, b)]) -}
	  >>> (first (runFork genr 0 {- ((k, b), WireMap) -}) -- Route via WireMap
	         >>> arr (\ ((kb,wm),lkb) -> (kb:lkb, wm)) {- ([(k, b)], WireMap) -}
	       <?> mkFix (\_ (((_, wm), lkb),e) -> Left ((e,lkb),wm)))

-- | Send Nothing to timed-out wires and collect their responses.
cleaner :: (Monad m, Ord k) => Time -> Wire e m (WireMap k m a b) ([(k,b)], (WireMap k m a b))
cleaner t' = mkGen $ \ dt wm -> do
	    let t = t' + dt
		run1 :: (Monad m, Ord k)
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
	    return (Right (lkb, rwm), cleaner t)

-- | The insertion time (t0) is the absolute expiration time.
-- dt < 0 => unexpired, >= 0 => expired.
runFork :: (Monad m, Ord k)
	=> Wire e m k (StrategyWire m a b)
	-> Time
	-> Wire e m ((k,a), (WireMap k m a b))
		    ((k,b), (WireMap k m a b))
runFork genr' t' = mkGen $ \ dt ((k,a), wm') -> do
	    let t = t' + dt
		{-run1 :: (Monad m, Ord k)
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
			return (Right ((k,b), wm), runFork genr' t)
		Nothing -> do
			(ew, genr) <- stepWire genr' dt k
			case ew of
			    Left err -> return (Left err, runFork genr t)
			    Right w -> do
				(b, mtrans) <- run1 (w, t)
				let wm = fromMaybe id mtrans wm' -- ignore on inh
				return (Right ((k,b), wm), runFork genr t)
	    --wm `seq` return (Right (b, wm), runStMap genr) -- require eval. of wm??

-- | A simple one-time response that doesn't require any strategy.
onceler  :: (Monad m) => (a -> Maybe b) -> StrategyWire m a (Maybe b)
onceler f = mkFix $ \ _ -> Left . (>>= f)

-- | Act like the identity wire until the timeout passes, then always inhibit.
--
-- * Depends: current instant when producing, time.
--
-- * Inhibits: until the given amount of time has passed.

timeOut :: (Monoid e) => Time -> Wire e m a a
timeOut t
    | t >= 0     = mkPure $ \dt a -> (Right a, timeOut (t - dt))
    | otherwise  = inhibit mempty



-- | Up-cast a regular StrategyWire to a NoClone wire.
noClone :: (Monad m, Ord k) => StrategyWire m a b -> CloneWire k m a b
noClone w = (w >>> arr (\(v,t) -> ((v,NoClone),t))) <?> mkFix (\ _ (_,r) -> Left (r,NoClone))

oncelerC :: (Monad m, Ord k) => (a -> Maybe b) -> CloneWire k m a (Maybe b)
oncelerC = noClone . onceler

-- | Act like a StrategyWire, but spawn a clone on the first return.
spawn :: (Monad m, Ord k)
      => (Time, k, CloneWire k m a b)
      -> StrategyWire m a b
      -> CloneWire k m a b
spawn cl w = mkGen $ \ dt ma -> do
	(eb, w') <- stepWire w dt ma
	let resp = case eb of
		   Left err -> Left (err, Clone cl)
		   Right (b,dt) -> Right ((b, Clone cl),dt)
	return (resp, noClone w')

-- | You fought with my father in the clone wars?
data CloneResp' k m a b = NoClone
		          | Clone (Time, k, CloneWire k m a b)
type CloneResp k m a b = (b, CloneResp' k m a b)

-- | The type of wire that returns either a response or
-- a witty remark plus instructions to build an army for the Republic.
type CloneWire k m a b = StrategyWire m a (CloneResp k m a b)

-- | Take you to him, I will.
type CloneMap k m a b = TimedMap Time k (CloneWire k m a b)

-- | cloneFork is a fork where handlers may install peers
-- (e.g. a walk creating a handler for the new fid.)
-- | The first arg. is a default wire, which can signal
-- if it wants to hang around to process subsequent reqs on channel 'k'.
cloneFork :: (Monad m, Ord k)
	  => Wire e m (k,a) (CloneResp k m a b)
	  -> Wire (e,[(k,b)]) m (k,a) [(k,b)]
cloneFork genr = loopArr Tm.empty $ {- ((k,a), CloneMap) -}
	second (cleanerC 0) {- ((k, a), ([(k,b)], CloneMap)) -}
	  >>> arr (\ (ka, (lb, wm)) -> ((ka,wm), lb)) {- (((k, a), CloneMap), [(k, b)]) -}
	  >>> (first (runForkC genr 0 {- ((k, b), CloneMap) -}) -- Route via CloneMap
	         >>> arr (\ ((kb,wm),lkb) -> (kb:lkb, wm)) {- ([(k, b)], CloneMap) -}
	       <?> mkFix (\_ (((_, wm), lkb),e) -> Left ((e,lkb),wm)))

-- | Send to Kamino to search for source of poison darts.
cleanerC :: (Monad m, Ord k)
	 => Time
	 -> Wire e m (CloneMap k m a b) ([(k,b)], (CloneMap k m a b))
cleanerC t' = mkGen $ \ dt wm -> do
	    let t = t' + dt
		{-run1 :: (Monad m, Ord k)
		    => ([(k,b)], CloneMap k m a b)
		    -> (k, (CloneWire m a b, Time))
		    -> m ([(k,b)], CloneMap k m a b)-}
	        run1 (lkb, wm') (k, (w0, t0)) = do
		    let dt = t - t0
		    (ec, w) <- dt `seq` stepWire w0 dt Nothing
		    return $ case ec of
			Right ((b,c),etn) -> ((k,b):lkb, (conscript c t . Tm.insert (t+etn) k w) wm')
			Left   (b,c)      -> ((k,b):lkb, (conscript c t . Tm.delete k) wm')
	    (lkb, rwm) <- foldM run1 ([],wm) (splitAssoc t wm)
	    return (Right (lkb, rwm), cleanerC t)

-- | Insert a clone into a CloneMap.
conscript :: (Monad m, Ord k)
	  => CloneResp' k m a b
	  -> Time
	  -> CloneMap k m a b
	  -> CloneMap k m a b
conscript NoClone _ x = x
conscript (Clone (dt,k,w)) t x = Tm.insert (t+dt) k w x

-- | This routine takes a default option and a map of clone wires
-- and runs whatever 'k' matches.  If the wire returns a 'Clone' value,
-- it modifies its routing table accordingly.
-- Wires stored for re-run will see a time delta from t0, their absolute
-- expiration time, so dt < 0 => unexpired, >= 0 => expired.
-- That departs from the spec., but is the most useful number
-- they could get.
-- TODO: coalesce all changes to the map and run 'em at once.
runForkC :: (Monad m, Ord k)
	=> Wire e m (k,a) (CloneResp k m a b)
	-> Time
	-> Wire e m ((k,a), (CloneMap k m a b))
		    ((k,b), (CloneMap k m a b))
runForkC genr' t' = mkGen $ \ dt ((k,a), wm') -> do
	    let t = t' + dt
		{-run1 :: (Monad m, Ord k)
                    => (CloneWire k m a b, Time)
		    -> (CloneMap k m a b -> CloneMap k m a b)
                    -> m (b, CloneMap k m a b)-}
                run1 (w0,t0) ondel = do
		    let dt = t - t0
		    (ec, w) <- dt `seq` stepWire w0 dt (Just a)
		    return $ case ec of
                        Right ((b,c),etn) -> (b, (conscript c t . Tm.insert (t+etn) k w) wm')
                        Left   (b,c)      -> (b, (conscript c t . ondel) wm')
	    
	    case Tm.lookup k wm' of
		Just wt0 -> do
                        (b, wm) <- run1 wt0 (Tm.delete k) -- delete on inh
			return (Right ((k,b), wm), runForkC genr' t)
		Nothing -> do
			(ec, genr) <- stepWire genr' dt (k,a)
			return $ case ec of
			    Right (b,c) -> let wm = conscript c t wm'
					   in (Right ((k,b), wm), runForkC genr t)
			    Left e  -> (Left e, runForkC genr t)

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

