{-# Language ExistentialQuantification #-}
import Data.Char
import Data.List
import Data.Binary.Put (runPut)
import Data.Binary.Get (runGet)
import Data.Text
import Control.Monad
import Test.QuickCheck
import Text.Printf

import Network.Plan9
import Blaze.ByteString.Builder (toLazyByteString)
import qualified Data.ByteString.Lazy as L

main  = mapM_ (\(s,a) -> printf "%-25s: " s >> a) tests

--deepCheck p = check (defaultConfig { configMaxTest = 10000}) p

-- reversing twice a finite list, is the same as identity
--prop_reversereverse s = (reverse . reverse) s == id s
--    where _ = s :: [Int]

-- and add this to the tests list
--tests  = [("reverse.reverse/id", quickCheck prop_reversereverse)]
tests = [("prop_correct_putsize", quickCheck prop_correct_putsize)]

instance Arbitrary Text where
    arbitrary = liftM pack arbitrary
instance Arbitrary Qid where
    arbitrary = liftM3 Qid arbitrary arbitrary arbitrary
instance Arbitrary Stat where
    arbitrary = liftM (\ x -> emptyStat{ st_name = x }) arbitrary
instance Arbitrary NineMsg where
    arbitrary = frequency [ (1, liftM2 Tversion arbitrary arbitrary),
			    (1, liftM2 Rversion arbitrary arbitrary) ]

prop_correct_putsize s = size s == (fromIntegral $ (L.length . runPut . putNinePkt) (12,s))
prop_correct_buildsize s = size s == (fromIntegral $ (L.length . toLazyByteString . buildNinePkt) (12,s))

prop_lossless_putget s = (12,s) == runGet getNinePkt (runPut $ putNinePkt (12,s))
prop_lossless_buildget s = (12,s) == runGet getNinePkt (toLazyByteString $ buildNinePkt (12,s))

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

