{-# Language ExistentialQuantification #-}
import Data.Char
import Data.List
import Data.Binary.Put (runPut, putWord32le, putLazyByteString)
import Data.Binary.Get (runGet, skip, getRemainingLazyByteString)
import Data.Text
import Data.Word
import Control.Monad
import Test.QuickCheck
import Text.Printf
import Data.Attoparsec (parse, IResult(..))

import Network.Plan9
import Blaze.ByteString.Builder (toLazyByteString)
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L

main  = mapM_ (\(s,a) -> printf "%-25s: " s >> a) tests

--deepCheck p = check (defaultConfig { configMaxTest = 10000}) p

-- reversing twice a finite list, is the same as identity
--prop_reversereverse s = (reverse . reverse) s == id s
--    where _ = s :: [Int]

-- and add this to the tests list
--tests  = [("reverse.reverse/id", quickCheck prop_reversereverse)]
--
tests = [("qid putsize", quickCheck (prop_putsize :: Qid -> Bool)),
	 ("stat putsize", quickCheck (prop_putsize :: Stat -> Bool)),
	 ("qid buildsize", quickCheck (prop_buildsize :: Qid -> Bool)),
	 ("stat buildsize", quickCheck (prop_buildsize :: Stat -> Bool)),
	 ("NineMsg putsize", quickCheck prop_ninemsg_putsize),
	 ("prop_lossless_putget", quickCheck prop_lossless_putget),
	 ("prop_lossless_buildget", quickCheck prop_lossless_buildget),
	 ("prop_parse_gets_sz", quickCheck prop_parse_gets_sz)]

instance Arbitrary Text where
    arbitrary = liftM pack arbitrary
instance Arbitrary L.ByteString where
    arbitrary = liftM L.pack arbitrary
instance Arbitrary Qid where
    arbitrary = liftM3 Qid arbitrary arbitrary arbitrary
instance Arbitrary Stat where
    arbitrary = do
	typ <- arbitrary
	dev <- arbitrary
	qid <- arbitrary
	mode <- arbitrary
	atime <- arbitrary
	mtime <- arbitrary
	length <- arbitrary
	name <- arbitrary
	uid <- arbitrary
	gid <- arbitrary
	muid <- arbitrary
	return $ Stat {
		st_typ = typ,
		st_dev = dev,
		st_qid = qid,
		st_mode = mode,
		st_atime = atime,
		st_mtime = mtime,
		st_length = length,
		st_name = name,
		st_uid = uid,
		st_gid = gid,
		st_muid = muid
	}
	-- liftM (\ x -> emptyStat{ st_name = x }) arbitrary
instance Arbitrary NineMsg where
    arbitrary = frequency [ (1, liftM2 Tversion arbitrary arbitrary),
			    (1, liftM2 Rversion arbitrary arbitrary),
			    (1, liftM3 Tauth arbitrary arbitrary arbitrary),
			    (1, liftM Rauth arbitrary),
			    (3, liftM Rerror arbitrary),
			    (2, liftM Tflush arbitrary),
			    (2, return Rflush),
			    (1, liftM4 Tattach arbitrary arbitrary arbitrary arbitrary),
			    (1, liftM Rattach arbitrary),
			    (4, liftM3 Twalk arbitrary arbitrary arbitrary),
			    (4, liftM Rwalk arbitrary),
			    (3, liftM2 Topen arbitrary arbitrary),
			    (3, liftM2 Ropen arbitrary arbitrary),
			    (3, liftM4 Tcreate arbitrary arbitrary arbitrary arbitrary),
			    (3, liftM2 Rcreate arbitrary arbitrary),
			    (2, liftM3 Tread arbitrary arbitrary arbitrary),
			    (2, liftM Rread arbitrary),
			    (2, liftM3 Twrite arbitrary arbitrary arbitrary),
			    (2, liftM Rwrite arbitrary),
			    (4, liftM Tclunk arbitrary),
			    (4, return Rclunk),
			    (2, liftM Tremove arbitrary),
			    (2, return Rremove),
			    (3, liftM Tstat arbitrary),
			    (3, liftM Rstat arbitrary),
			    (3, liftM2 Twstat arbitrary arbitrary),
			    (3, return Rwstat) ]

prop_putsize :: (Sized a, Bin a) => a -> Bool
prop_putsize s = size s == (fromIntegral $ (L.length . runPut . put) s)

prop_buildsize :: (Sized a, Buildable a) => a -> Bool
prop_buildsize s = size s == (fromIntegral $ (L.length . toLazyByteString . build) s)

prop_ninemsg_putsize s = size s == (fromIntegral $ (L.length . runPut . putNinePkt) (12,s))
prop_ninemsg_buildsize s = size s == (fromIntegral $ (L.length . toLazyByteString . buildNinePkt) (12,s))

prop_lossless_putget :: (Word16, NineMsg) -> Bool
prop_lossless_putget s = s == (decodeNinePkt . encodeNinePkt) s
prop_lossless_buildget s = s == (decodeNinePkt . toLazyByteString . buildNinePkt) s

prop_parse_gets_sz :: (Word16, NineMsg) -> Bool
prop_parse_gets_sz msg = let pkt = (toLazyByteString . buildNinePkt) msg
			     sz = size pkt
			     p = parse (parseNinePkt (sz*2)) ((S.concat . L.toChunks) pkt)
			     pktC = (S.concat . L.toChunks) $ runGet
					(skip 4 >> getRemainingLazyByteString) pkt
			 in case p of
				Done t pktC'   -> pktC == pktC' && size t == 0
				_  -> False

{-
data Blech = Foo Int | Bar String Blech
	deriving (Show)

tree = sized tree'
tree' 0 = liftM Foo arbitrary
tree' n | n>0 = 
	oneof [liftM Foo arbitrary,
	       liftM2 Bar arbitrary subtree]
  where subtree = tree' (n - 1)

instance Arbitrary Blech where
    arbitrary     = tree

testblech b = case b of Foo _ -> True; _ -> False
-}

