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
    ename :: String,
--    errno :: Word32, -- 9P2000
    reset :: m ()     -- always have an exit strategy to recover the server
}
-- | Report the error, but don't do any monadic action.
sError :: (Monad m) => String -> ServerError m
sError err = SError err (return())

-- | Pre-specify error and monad types
-- all the clutter is because we parameterize over m,
-- the monad type, so users can do IO / state
-- violence in there.
type SrvWire m = Wire (ServerError m) m

data AttReq = AttReq {
--  fid   :: Word32,
--  afid  :: Word32,
  uname :: String,
  aname :: String
--  n_uname :: Word32 -- 9P2000
}
{- |  The most signifigant change to the create operation is the new permission
 - modes which allow for creation of special files.  In addition to creating
 - directories with DMDIR, 9P2000.u allows the creation of symlinks (DMSYMLINK),
 - devices (DMDEVICE), named pipes (DMNAMEPIPE), and sockets (DMSOCKET).
 - extension[s] is a string describing special files, depending on the mode bit.
 - For DSYMLINK files, the string is the target of the link. For DMDEVICE files,
 - the string is "b 1 2" for a block device with major 1, minor 2. For normal
 - files, this string is empty. 
 -}
data CreateReq = CreateReq {
  name :: String,
  perm :: Word32,
  mode :: Word8
--  exts :: String
}
data OpenResp = OpenResp {
  qid :: Qid,
  iounit :: Word32
}

-- | Handlers forming the Server Pipeline
data FileSysH m = FileSysH {
    exts   :: [String], -- extensions accepted by the server (e.g. ".u" for 9P2000)
    attach :: (SrvWire m) AttReq (FidH m, NSH m)
}
fileSysH (FileSysH exts attach) = self
    where self = mkGen $ \ dt msg ->
	case msg of Tversion sz ex -> return $ Rversion sz ex
		Tattach  a b c -> do
			(minfo, at') <- stepWire attach dt (AttReq a b c)
			case minfo of
				Right (qid, fh, nsh) -> need to add to map here...
				Left err -> return (Left err, fileSysH (FileSysH exts at'))
		ow -> do
			need to run map here

-- | 1. This says how to strip the leading routing info. from the packet.
--   2. This provides a 'new handler' function for never-before-seen routes,
--      and defaults to those new routes in the future.
--       - this is used advantageously to separate the routing into stages
--       with the same logical structure.
--   3. It allows the handler creation to change as new handlers are added.
fileSysH (FileSysH exts attach) = demux_fid >>> fork $ mkGen $ \ _ (fid, att) ->
	case att of
	    Tattach a b c -> do
		(minfo, at') <- stepWire attach dt (AttReq a b c)
		case minfo of
			Right (fidh, nsh) -> return $ nsH fidh nsh
			Left err -> return (Left err, fileSysH (FileSysH exts at'))
	    _ -> return (inhibit "EINVAL: Unrecognized fid.")

nsH fidh (NSH walk create remove detach) = demux_fid >>> fork $ mkGen $ \ _ (fid, att) ->
	

fidH (FidH stat wstat open clunk) =

data NSH m = NSH {
    walk   :: (SrvWire m) [String] ([Qid], FidH m),
    create :: (SrvWire m) CreateReq (OpenResp, FidH m),
    remove :: (SrvWire m) () (),
    detach :: (SrvWire m) () () -- connection shutdown function, called after all clunks.
}
data FidH m = FidH {
    stat  :: (SrvWire m) () Stat,
    wstat :: (SrvWire m) Stat (),
    open  :: (SrvWire m) Word8 (OpenResp, OpenFidH m),
    clunk :: (SrvWire m) () ()
}

-- TODO: check what we can do to make these interruptable when flush comes along.
data OpenFidH m = OpenFidH {
    read :: (SrvWire m) (Word64,Word32) (Word32,ByteString),
    write :: (SrvWire m) (Word64,Word32,ByteString) Word32
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
-- This wire keeps an internal state listing all `attached' NameSpace handlers (NSH-s)
-- and tags routed to them.
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

data SrvWire = SrvWire 

fork (simple <|> fork w)

instance Monad ServerWire where
  return a = 
  a >>= b = fork $ mkFix ()

-- | Fork to handle ea. fid separately.
fidReq = fork $ arr (fid -> translateA msg)
  where translateA = mkFix $ \ _ -> translate
        translate msg = case msg of
	    Twalk fid _ -> (fid, Walkto _)

forkServer :: Wire (ServerError m) m Word16 (StrategyWire m NineMsg NineMsg)
forkServer :: (Monad m) => (SrvWire m) AttReq (Qid, NSH m) -> 
forkServer attach = 

type FidHandlerWire m :: Wire (ServerError m) m NineMsg (StrategyWire m NineMsg NineMsg)
attHandler :: FidHandlerWire m

-- | * Example RamDisk Server:

ramDisk = FileSysH [] attach
ramDisk = mkServer ramDisk

{-
mkNSH :: (Monad m) => NineMsg -> NineMsg

---------------- The following routines work on individual fids -------------

-- Return an OpenFileWire by adding read/write functions atop a standard file wire
type FileWire m :: (Monad m) => (SrvWire m) FileReq NineMsg
mkFile :: (Monad m) => FileWire -> FileWire

mkOpenFile :: (Monad m) => FileWire -> FileWire
-}
