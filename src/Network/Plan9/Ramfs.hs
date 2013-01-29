module Network.Plan9.Ramfs where

import Network.Plan9.Server
import Control.Monad.Error
import qualified Data.Map as M
import qualified Monad.State as ST

newtype PathElem = Text

-- | Unfortunately these must all be able to return lookup errors.
-- `Se la vie!' say the old folks.
class (NineFile e d, NineFile e f)
	=> NineDir e d f | d -> f where
    walk   :: PathElem -> d -> e d
    create :: Word64 -> PathElem -> d -> e (d, f) -- | gets passed a unique qid_path
    remove :: PathElem -> d -> e d
    list   :: d -> [f]

class (Error e) => NineFile e f where
    wstat :: Stat -> f -> e f
    stat  :: f -> Stat
--    open  :: f -> mode -> ()
--    clunk :: f -> ()

class (Error e) => Readable e a where
    read :: a -> (Word64,Word32) -> e ByteString

class (Error e) => Writeable e a where
    write :: (Word64,ByteString) -> a -> e a

-- | Here are the implementation details for making
-- the structure a real linked list.

class (STMFile e d, STMFile e f)
	=> STMDir e d f | d -> f where
    mwalk :: PathElem -> STMVar d -> e (Qid, STMVar d)
    mcreate :: Word64 -> PathElem -> STMVar d -> e (STMVar f)
    mremove :: PathElem -> STMVar d -> e ()
    mlist   :: STMVar d -> [STMVar f]

class (Error e) => STMFile e f where
    mwstat :: Stat -> STMVar f -> STM(e ())
    mstat  :: STMVar f -> Stat

instance (NineFile e f) => STMFile e f where
    mwstat st sf = runErrorT $ atomically $ do
	v <- liftIO $ readTVar sf
	v' <- wstat st v
	liftIO $ writeTVar sf v'
    mstat = stat . atomRead

atomRead = atomically . readTVar
appV fn x = atomically $ readTVar x >>= writeTVar x

instance (NineDir e d f) => STMDir e d f where
    mwalk name sd = atomically $ do
	d <- copyVar sd
	case walk name d of
	    err -> err
	    ok d' -> return (mkSTVar d', st_qid(stat d'))
    ...

-- | The canonical factorial function.

type MulTree = Int
instance NineFile (Either Text) MulTree where
    stat n = emptyStat{stname = show n, st_mode = dmdir, st_qid = emptyQid{qid_typ = qtdir}}
    wstat _ n = return n
instance NineDir (Either Text) MulTree MulTree where
    walk lst n = return $ product (n:lst)
    create _ n d = return (d, n * d)
    remove _ d = return d
    list   _   = []

-- | The canonical example of a filespace in RAM.
{-
data FileTree' a = Dir (M.Map String (FileTree a))
		 | File a
type FileTree a = (Stat, FileTree' a)

instance NineFile (Either Text) (FileTree a) where
    stat (s, _) = s
    wstat s (_, a) = (s, a)
instance NineDir (Either Text) (FileTree a) (FileTree a) where
    walk = fst_walk 
    create pathn n d = second (first \st -> st{(st_qid st){qid_path=pathn}}) <$> fst_create n d
    remove = fst_remove
    list (_, File a) = throwError "ENOTDIR: Can't list contents."
    list (_, Dir m)  = return $ M.listKeys m

type FState a = ST.State (FileTree a)
type FOp m a a' = Either (ServerError m) (FileTree a, a')

fst_walk :: [String] -> a -> FState (FileTree a)
fst_walk _ (File _) = Left $ sError "EBADUSE: File is not a directory."
fst_walk [] d      = FState (FileTree a)
fst_walk [hd:tl] d = FState (FileTree a)

fst_create :: [String] -> a -> FileTree a -> Fop m a a'
fst_create _ _ (File _) = Left $ sError "EBADUSE: File is not a directory."
fst_create [] _ _ = Left $ sError "EINVAL: Can't re-create /."
fst_create [name] x (Dir m) = Dir $ M.add name x m
fst_create [hd:tl] x (Dir m) =
	let mf = M.lookup hd m >>= create tl x
	in case mf of
		Just f -> Dir $ M.add hd f m
		Nothing -> sError "EINVAL: Bad Path"

fst_remove :: [String] -> FileTree a -> Fop m a a'
fst_remove [] _ = Left $ sError "EINVAL: Can't remove /."
fst_remove _ (File _) = Left $ sError 
-}

-- | The implementation of NineDir / NineFile classes:
-- Since these are operators acting on the file tree, they
-- are built-to-order to serve up a given NineDir class member.
--
--   This means that the recursion works naturally when
-- the object is converted into an arrow, able to dispatch
-- on its input `call'.
--
--   TODO: can we also use the structure of the arrow to stop the
-- server and get the resulting object back?

ramfs = FileSysH {
    fsexts   = [],
    fsattach = arr (\ uname aname -> emptyQid, fidh [])
}

fsErr = inhibit $ sError "FileSystem Error"
iounit = 4096 :: Word32

-- | 9P2000 interfacing with a tree implemented in STM:
-- | The handler for namespace operations starting at 'path'
nsh :: (NineDir e a a) => STMVar a -> NSH (STM())
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
fidh :: (STMTree Text a) => STMVar a -> FidH (STM())
fidh path = FidH {
    fnsh    = nsh path,
    fstat   = arr (\ _ -> emptyStat),
    fwstat  = arr (\ stat -> ()),
    fopen   = arr (\ mode -> (myQid qid, iounit, openFidH path)),
    fclunk  = mkPure (\ _ _ -> (Right (), fsErr))
}

openFidH path = OpenFidH {
    ofidh  =  fidh path ,
    ofread  = (SrvWire m) (Word64,Word32) (Word32,ByteString),
    ofwrite = (SrvWire m) (Word64,Word32,ByteString) Word32
}

-- | serve9p ramfs

