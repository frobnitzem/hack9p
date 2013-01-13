{-# Language ExistentialQuantification #-}
import Data.Char
import Data.List
import Data.Binary.Put (runPut, putWord32le, putLazyByteString)
import Data.Binary.Get (runGet, skip, getRemainingLazyByteString)
import Data.Text
import Data.Word
import Control.Monad
import Data.Monoid (Monoid(..))
import Test.QuickCheck
import Text.Printf
import Data.Attoparsec (parse, IResult(..))

import Network.Plan9
import Control.Wire (stepWire, Time, Wire)
import qualified Control.Monad.State as SM
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

-- How to test a session?
-- 1. It's like programming a computer to play a game,
--    and observe the rules.
-- 2. We describe a stateful system with a chat-script,
--    like a Turing challenge from machine to machine.
-- 3. Our chat-script is an arrow, taking a response
--    and producing a request. -- exactly the same format
--    as the server.
-- 4. The difference is that our arrow can end the challenge
--    at any time with a Nothing, denoting no detected
--    abnormality, or a Just (to say...), denoting a failed
--    expectation.

-- Carry out a conversation, kicked off with an initial send
-- from the client to the server, run server, run client, etc.
-- It ends when the client (or server) inhibits
-- Either 'stepwire' can modify the state to, e.g. journal
--   the transactions.
chat :: a -- hello
     -> Wire srverr (SM.State s) a b -- server
     -> Wire clierr (SM.State s) b a -- client
     -> SM.State s (Either srverr clierr)
chat hello srv' cli' = do
	(sresp, srv) <- stepWire srv' 1 hello
	case sresp of
	    Right resp -> do
		(cresp, cli) <- stepWire cli' 1 resp
		case cresp of
			Right ok -> chat ok srv cli
			Left cerr -> return $ Right cerr
	    Left serr -> return $ Left serr

-- | This one also requires the client to have a maybe
-- error to represent whether it just got tired.
showChat :: (Show s, Monoid s, Show srverr, Show clierr)
        => a -- hello
        -> Wire srverr (SM.State s) a b -- server
        -> Wire (Maybe clierr) (SM.State s) b a -- client
        -> String
showChat a srv cli =
	let (err, s) = SM.runState (chat a srv cli) mempty
	    res = case err of
		Left err -> "Server Error: " ++ show err
		Right merr -> maybe "Success!"
				    (("Client Error: " ++) . show) merr
	in show s ++ "\n" ++ res ++ "\n"

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

