{-# LANGUAGE OverloadedStrings, TupleSections #-}
-- |
-- Module:     Network.Plan9.Server
-- Copyright:  (c) 2013 David M. Rogers
-- License:    GPL-3
-- Maintainer: David M. Rogers <predictivestatmech@gmail.com>
--
-- FRP framework for a file-server.

module Network.Plan9.Server
    where

import Control.Wire
import Data.Word
import Data.Maybe (mapMaybe)
import Data.ByteString.Lazy (ByteString)
import Control.Arrow

import qualified Data.Text as T

import Network.Plan9.Consts
import Network.Plan9.Parser
import Network.Plan9.Builder
import Network.Plan9.Wires

import Prelude hiding (id,(.))

data Server = Server {
    ver :: String,
    defMsize :: Int
}

data ServerError m = SError {
    serr_name :: String,
--    errno :: Word32, -- 9P2000
    serr_reset :: m ()     -- always have an exit strategy to recover the server
}
-- | Report the error, but don't do any monadic action.
sError :: (Monad m) => String -> ServerError m
sError err = SError err (return())

-- | Pre-specify error and monad types
-- all the clutter is because we parameterize over m,
-- the monad type, so users can do IO / state violence in there.
type SrvWire m = Wire (ServerError m) m

-- | Handlers forming the Server Pipeline
data FileSysH m = FileSysH {
    fsexts   :: [String], -- extensions accepted by the server (e.g. ".u" for 9P2000)
    fsattach :: (SrvWire m) (String,String) (Qid, FidH m)
}
{- Without utilizing fork (to illustrate the problems)
fileSysH (FileSysH exts attach) = self
    where self = mkGen $ \ dt msg ->
	case msg of Tversion sz ex -> return $ Rversion sz ex
		Tattach  a b c -> do
			(einfo, at') <- stepWire attach dt (AttReq a b c)
			case einfo of
				Right (qid, fh) -> need to add to map here...
				Left err -> return (Left err, fileSysH (FileSysH exts at'))
		ow -> do
			need to run map here
-}
-- | 1. This says how to strip the leading routing info. from the packet.
--   2. This provides a 'new handler' function for never-before-seen routes,
--      and defaults to those new routes in the future.
--       - this is used advantageously to separate the routing into stages
--       with the same logical structure.
--   3. It allows the handler creation to change as new handlers are added.
--   The file system handler pulls the fid values from all input packets,
--   and routes them to the appropriate, established, fid-handlers.
--   The cloneFork allows those fid handlers to insert new fid handlers
--   as peers.
--     That means the only message to handle at this level is
--   the attach message.  It also means that the server departs from the Plan9
--   spec in that open fids will remain valid even if the aname is re-attached.
fileSysH :: FileSysH -> SrvWire m NineMsg [Maybe (Word32, NineMsg)]
fileSysH (FileSysH exts attach) = demux_fid >>> cloneFork ( arr
	(\ _ {-new fid-} -> mkGen $ \ dt msg ->
	  case msg of
	    Tattach _ afid uname aname -> do
		(einfo, at') <- stepWire attach dt (uname, aname)
		case einfo of
			Right (qid, fidh) -> return $
				((Just . Right) (Rattach qid, NoClone), fidh)
			Left err -> return (Left err, fileSysH (FileSysH exts at'))
	    _ -> return (Left "EINVAL: Unrecognized fid.", fileSysH (FileSysH exts at'))
	)) >>> arr (>>= snd)

-- | Operations referencing the current Fid.
data FidH m = FidH {
    fwalk   :: SrvWire m [String] ([Qid], FidH m),
    fcreate :: SrvWire m (String, Word32, Word8) (Qid, Word32, FidH m),
    fremove :: SrvWire m () (),

    fstat   :: SrvWire m () Stat,
    fwstat  :: SrvWire m Stat (),
    fopen   :: SrvWire m Word8 (Qid, Word32, OpenFidH m),
    fclunk  :: SrvWire m () ()
}

-- | Structure which is passed to a CaseWire
-- The output of the function is a tuple of the seed value for the wire,
-- the destructor and constructor for FidH, and an arrow to process its output.
-- ( a, (NSH m -> SrvWire m a b),
--  (SrvWire m a b -> NSH m -> NSH m),
--  CloneWire k m b (Maybe c) )
fidH :: (Monad m, Ord k)
	    => FidH -> CloneWire k m NineMsg 
fidH fidh = caseWire fid_op fidh
  where fid_op msg = case msg of
	    Twalk   _ nw names -> (names,
				   fwalk,
				   (\ w fh -> fh{fwalk=w}),
				   oncelerC $ \(lq,fidh) -> (Rwalk lq, Clone $ fidH fidh)
	    			  )
	    Tcreate _ name perm mode -> ((name, perm, mode),
					 fcreate,
					 (\ w fh -> fh{fcreate=w}),
					 oncelerC $ \() -> (Rcreate qid iounit, Done)
				        )
	    Tremove _ -> ((),
			  fremove,
			  (\ w fh -> fh{fremove=w}),
			  oncelerC $ \ _ -> (Rremove, Done)
			 )
	    Tstat   _ -> ((),
			  fstat,
 			  (\ w fh -> fh{fstat=w}),
			  oncelerC $ \ _ -> (Rstat stat, Done)
			 )
	    Twstat  _ stat -> (stat,
			       fwstat,
			       (\ w fh -> fh{fwstat=w}),
			       oncelerC $ \ _ -> (Rwstat, Done)
	    		      )
	    Topen   _ mode -> (mode,
			       fopen,
			       (\ w fh -> fh{fopen=w}),
			       oncelerC $ \() -> (Ropen qid iounit, Done)
			      )
	    Tclunk  _ -> ((),
			  fclunk,
			  (\ w fh -> fh{fclunk=w}),
			  oncelerC $ \ _ -> (Rclunk, Done)
		         )
	    n -> (n, ferror, (\ w fh -> fh{ferror=w}), id)

data OpenFidH m = OpenFidH {
    ofidh :: FidH m,
    ofread :: (SrvWire m) (Word64,Word32) (Word32,ByteString),
    ofwrite :: (SrvWire m) (Word64,Word32,ByteString) Word32
}

{- The main body of the server specifies the hard and
 - fast Plan9 rules, using a simple flow-path:
 - FileSysH -> NameSpaceH -> FidH -> OpenFidH
 -}

-- Top-level function, returning a network IO wire from
-- a FileSysH
-- It looks like: (IO) >>> sort-by-tag [ < Sort-by-(NS/File op) |NS -> send to NSH   >]-> Assemble Tag
--                                       < |file -> sort-by-fid --> send to FileWire >
--                                  -> Check for Error and send RError if so.
-- an error thrown at this level indicates an internal server error.
-- 
-- This wire keeps an internal state listing all 'live' tags, and routes to them.
-- Mostly, it will run the main wire, which creates a handler for a new tag.
-- type StrategyWire m a b = Wire b m (Maybe a) (b,Time)
-- fork :: (Monad m, Ord k)
--      => Wire e m k (StrategyWire m a b)
--      -> Wire (e,[(k,b)]) m (k,a) [(k,b)]
type ServerWire m = Wire (ServerError m) m NinePkt [(Word16, NineMsg)]
mkServer :: (Monad m) => FileSysH m -> ServerWire m
mkServer (FileSysH exts attach) =
	(fork $ arr (\_ -> mkFix (\_ -> maybe end serve))) {- [(Word16, Maybe NineMsg)] -}
		>>> arr (mapMaybe (\(a,b) -> b >>= Just . (a,)))
    where
       serve (Tversion sz "9P2000") = Left . Just $ Rversion (maxsz sz) "9P2000"
       serve (Tversion sz ver) = case T.take 7 ver of
		"9P2000." -> Left . Just $ Rversion (maxsz sz) "9P2000"
		_	  -> Left . Just $ Rerror "Unsupported Protocol."
       serve _ = Left . Just $ Rerror "Unrecognized cmd."
       maxsz = max 65536
       end = Left Nothing

