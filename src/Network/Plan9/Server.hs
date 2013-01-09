-- |
-- Module:     Network.Plan9.Server
-- Copyright:  (c) 2013 David M. Rogers
-- License:    GPL3
-- Maintainer: David M. Rogers <predictivestatmech@gmail.com>
--
-- FRP framework for a file-server.

module Network.Plan9.Server
    where

import Control.Wire.Wire
import Data.Word

import Network.Plan9.NineP

data Server = Server {
    ver :: String,
    defMsize :: Int
}

data ServerError m = SError {
    ename :: String,
    errno :: Word32, -- 9P2000
    reset :: m()     -- always have an exit strategy to recover the server
}
-- Report the error, but don't do any monadic action.
sError :: String -> ServerError
sError err = SError err (return())

-- Pre-specify error and monad types
-- all the clutter is because we parameterize over m,
-- the monad type, so users can do IO / state
-- violence in there.
newtype SrvWire m = Wire (ServerError m) m

data AttReq = AttReq {
  fid   :: Word32,
  afid  :: Word32,
  uname :: String,
  aname :: String,
  n_uname :: Word32 -- 9P2000
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
  mode :: Mode,
  exts :: String
}
data OpenResp = OpenResp {
  qid :: Qid,
  iounit :: Word32
}

data FileSysH m = FileSysH {
    exts   :: String, -- extensions accepted by the server (e.g. ".u" for 9P2000)
    attach :: (SrvWire m) AttReq (Qid, NSH m)
}
data NSH m = NSH {
    walk   :: (SrvWire m) [String] ([Qid], FidH m),
    create :: (SrvWire m) CreateReq (OpenResp, FidH m),
    remove :: (SrvWire m) () (),
    detach :: (SrvWire m) () () -- connection shutdown function, called after all clunks.
}
data FidH = FidH {
    stat  :: (SrvWire m) () Stat,
    wstat :: (SrvWire m) Stat (),
    open  :: (SrvWire m) Mode (OpenResp, OpenFidH),
    clunk :: (SrvWire m) () ()
}

type Mode = Word8
type Offset = Offset Word64
type Count = Count Word32

-- TODO: check what we can do to make these interruptable when flush comes along.
data OpenFidH = OpenFidH {
    read :: (SrvWire m) (Offset,Count) (Count,ByteString),
    write :: (SrvWire m) (Offset,Count,ByteString) Count
}

{- The main body of the server specifies the hard and
 - fast Plan9 rules, using a simple flow-path:
 - FileSysH -> NameSpaceH -> FidH -> OpenFidH
 -}

-- Top-level function, returning a network IO wire from
-- a FileSystemH
-- It looks like: (IO) --> sort-by-tag -> Sort-by-(NS/File op) |NS -> send to NSH -> Assemble Tag
--                                        \ |file -> sort-by-fid --> send to FileWire -> Assemble Tag
--                                  -> Check for Error and send RError if so.
-- an error thrown at this level indicates an internal server error.
-- 
-- This wire keeps an internal state listing all `attached' NameSpace handlers (NSH-s)
-- and tags routed to them.
type ServerWire = Wire String m NinePkt NinePkt
mkServer :: (Monad m) => FileSystemH m -> ServerWire
mkServer (FileSystemH exts attach) = muxtag serve
    where
       serve :: Wire String m NineMsg NineMsg
       serve (TVersion sz ver) = lift (Rversion (name exts))
           where
               name [] = "9P2000"
               name exts  = "9P2000."++exts
       serve _ = sError "Unrecognized cmd."

       muxtag :: Wire String m NineMsg NineMsg -> Wire String m NinePkt NinePkt
       muxtag w = lift do
           

{-
mkPure :: (Time -> a -> (Either e b, Wire e m a b)) -> Wire e m a b

mkState ::
    s
    -> (Time -> (a, s) -> (Either e b, s))
    -> Wire e m a b
-}

mkNSH :: (Monad m) => NineMsg -> NineMsg

---------------- The following routines work on individual fids -------------

-- Return an OpenFileWire by adding read/write functions atop a standard file wire
type FileWire m :: (Monad m) => (SrvWire m) FileReq NineMsg
mkFile :: (Monad m) => FileWire -> FileWire

mkOpenFile :: (Monad m) => FileWire -> FileWire

