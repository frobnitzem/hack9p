module Network.Plan9.Ramfs where

import Network.Plan9.Server
import qualified Data.Map as M
import qualified Monad.State as ST

data FileTree a = Dir (M.Map String (FileTree a))
		| File a

type FState a = ST.State (FileTree a)
type FOp m a a' = Either (ServerError m) (FileTree a, a')

create :: [String] -> a -> FileTree a -> Fop m a a'
create _ _ (File _) = Left $ sError "EBADUSE: File is not a directory."
create [] _ _ = Left $ sError "EINVAL: Can't re-create /."
create [name] x (Dir m) = Dir $ M.add name x m
create [hd:tl] x (Dir m) =
	let mf = M.lookup hd m >>= create tl x
	in case mf of
		Just f -> Dir $ M.add hd f m
		Nothing -> sError "EINVAL: Bad Path"

remove :: [String] -> FileTree a -> Fop m a a'
remove [] _ = Left $ sError "EINVAL: Can't remove /."
remove _ (File _) = Left $ sError 

ramfs = FileSysH {
    fsexts   = [],
    fsattach = arr (\ uname aname -> emptyQid, fidh [])
}

fsErr = inhibit $ sError "FileSystem Error"
iounit = 4096 :: Word32

-- | The handler for namespace operations starting at 'path'
nsh path = NSH {
    nswalk   = mkGen $ \ _ names -> do let path' = append_path path names
				    qids <- walk path'
				    return (Right qids, fidh path'),
    nscreate = mkGen $ \ _ (name, perm, mode) ->
				    if ck_name name
				    then return (Left $ sError, nscreate $ nsh path)
				    else let path' = path++[name] in
					do qid <- create path' mempty
					   return (Right (qid, iounit, fidh path'), nscreate $ nsh path),
    nsremove = mkGen $ \ _ _ -> (remove path, fsErr)
}

-- | The handler for fid operations on 'path'
fidh path = FidH {
    fnsh    = nsh path,
    fstat   = arr (\ _ -> emptyStat),
    fwstat  = arr (\ stat -> ()),
    fopen   = arr (\ mode -> (myQid qid, iounit, openFidH path)),
    fclunk  = mkPure (\ _ _ -> (Right (), fsErr))
}

class (File9 b) => Dir9 a b where
    walk   :: a -> [b] -> a
    create :: a -> b -> a
    remove :: a -> b -> a

class File9 a where
    stat  :: a -> Stat
    wstat :: a -> Stat -> a
--    open  :: a -> Stat 
--    clunk :: a -> Stat

class Readable a where
    read :: a -> (Word64,Word32) -> ByteString

class Writeable a where
    write :: (Word64,ByteString) -> a -> a

openFidH path = OpenFidH {
    ofidh  =  fidh path ,
    ofread  = (SrvWire m) (Word64,Word32) (Word32,ByteString),
    ofwrite = (SrvWire m) (Word64,Word32,ByteString) Word32
}

serve9p ramfs

