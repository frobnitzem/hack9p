{-# LANGUAGE FlexibleInstances, TypeSynonymInstances, OverloadedStrings #-}
--module Network.Plan9.Parser (
module Parser (
                  -- ** buildNineMsg - function that can encode all NineMsg types to a Blaze.Builder
                   buildNinePkt

                  -- ** parseNineMsg - function to decode all NineMsg types from an input stream
                  , parseNinePkt
) where

import Blaze.ByteString.Builder.ByteString (copyByteString, fromByteString, fromLazyByteString)
import Blaze.ByteString.Builder (Builder, toLazyByteString, Write, Builder, fromWrite)
import Blaze.ByteString.Builder.Char.Utf8 (fromString)
import Blaze.ByteString.Builder.Word (
	fromWord16le, writeWord8, writeWord16le,
	writeWord32le, writeWord64le
	)

import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Data.Text (Text)
import qualified Data.Text as T

import Data.Word
import Control.Applicative ((<$>))
import Data.Attoparsec ((<?>), Parser)
import Data.Attoparsec.Binary (anyWord32le)
import qualified Data.Attoparsec as Atto
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L

import Network.Server.ScalableServer
--import Network.Plan9.NineP
import NineP

import Data.Monoid ((<>),mconcat)

class Writeable a where
    write :: a -> Write
instance Writeable Word8 where
    write = writeWord8
instance Writeable Word16 where
    write = writeWord16le
instance Writeable Word32 where
    write = writeWord32le
instance Writeable Word64 where
    write = writeWord64le
instance Writeable Qid where
    write (Qid typ vers path) = write typ <> write vers <> write path
instance Writeable MsgTyp where
    write = writeWord8 . fromIntegral . typInt

class Buildable a where
    build :: a -> Builder
instance Buildable Builder where
    build = id
instance Buildable Write where
    build = fromWrite
instance Buildable Text where
    build xs = let bs = encodeUtf8 xs
               in fromWord16le (fromIntegral $ S.length bs)
	          <> copyByteString bs
instance Buildable Stat where
    build (Stat typ dev qid mode atime mtime length name uid gid muid) =
      build (mconcat $ [write typ, write dev, write qid, write mode, write atime, write mtime,
                        write length])
            <> (mconcat $ map build [name, uid, gid, muid])

instance (Buildable a) => Buildable [a] where
    build = mconcat . (map build)

-- 9P2000 Message serialization size.
class Sized a where
  size :: a -> Int

instance (Sized a, Sized b) => Sized (a, b) where
  size (left, right) = size left + size right
instance Sized a => Sized [a] where
  size = sum . map size

--  <|> atEnd
parseNinePkt :: Int -> Parser (S.ByteString)
parseNinePkt maxSize = (<?> "NinePkt") $ do
  sz <- fromIntegral <$> anyWord32le
  if sz < 4+1+2 || sz > maxSize then
    fail $ "Invalid NinePkt size: " ++ (show sz)
  else
    Atto.take (sz-4)

writeLen :: [a] -> Write
writeLen = writeWord16le . fromIntegral . length
write32 :: Int -> Write
write32 = writeWord32le . fromIntegral

buildNinePkt :: (Int, NineMsg) -> Builder
buildNinePkt (tag,msg) = let sz = fromIntegral $ size msg
                             hdr = \ typ -> writeWord32le sz <> write typ <> writeWord16le (fromIntegral tag)
   in msgbuilder hdr
   where
      msgbuilder hdr = case msg of
	Tversion a b -> build (hdr TTversion <> write a) <> build b
	Rversion a b -> build (hdr TRversion <> write a) <> build b
	Tauth a b c -> build (hdr TTauth <> write a) <> build b <> build c
	Rauth a     -> build (hdr TRauth <> write a)
	Rerror a -> build (hdr TRerror) <> build a
	Tflush a -> build $ hdr TTflush <> write a
	Rflush   -> build $ hdr TRflush
	Tattach a b c d -> build (hdr TTattach <> write a <> write b) <> build c <> build d
	Rattach a       -> build $ hdr TRattach <> write a
	Twalk a b c -> build (hdr TTwalk <> write a <> write b <> writeLen c)
			<> mconcat (map build c)
	Rwalk a     -> build (hdr TRwalk <> writeLen a)
			<> mconcat (map (build . write) a)
	Topen a b -> build $ hdr TTopen <> write a <> write b
	Ropen a b -> build $ hdr TRopen <> write a <> write b
	Tcreate a b c d -> build (hdr TTcreate <> write a) <> build b <> build (write c <> write d)
	Rcreate a b     -> build $ hdr TRcreate <> write a <> write b
	Tread a b c -> build $ hdr TTread <> write a <> write b <> write c
	Rread a     -> build (hdr TRread <> write32 (size a)) <> fromLazyByteString a
	Twrite a b c -> build (hdr TTwrite <> write a <> write b <> write32 (size c)) <> fromLazyByteString c
	Rwrite a     -> build $ hdr TRwrite <> write a
	Tclunk a -> build $ hdr TTclunk <> write a
	Rclunk   -> build $ hdr TRclunk
	Tremove a -> build $ hdr TTremove <> write a
	Rremove   -> build $ hdr TRremove
	Tstat fid  -> build $ hdr TTstat <> write fid
	Rstat stat -> build (hdr TRstat) <> build stat
	Twstat fid stat -> build (hdr TTwstat <> write fid) <> build stat
	Rwstat          -> build $ hdr TRwstat

-- | Some drab utility functions.

instance Sized Word8 where
    size _ = 1
instance Sized Word16 where
    size _ = 2
instance Sized Word32 where
    size _ = 4
instance Sized Word64 where
    size _ = 8
instance Sized String where
    size = (+2) . size . fromString
instance Sized S.ByteString where
    size = S.length
instance Sized L.ByteString where
    size = fromIntegral . L.length
instance Sized Builder where
    size = size . toLazyByteString

instance Sized Stat where
    size (Stat typ dev qid mode atime mtime length name uid gid muid) =
      2 + sum [size typ, size dev, size qid, size mode, size atime, size mtime,
		size length, size name, size uid, size gid, size muid]
instance Sized Qid where
    size (Qid a b c) = size a + size b + size c
instance Sized Text where
    size t = 2 + T.length t
    
instance Sized NineMsg where
    size msg = (4 + 1 + 2) + case msg of
	(Tversion msize ver) -> 4 + size ver
        (Rversion msize ver) -> 4 + size ver

        (Tauth afid uname aname) -> 4 + size uname + size aname
        (Rauth qid) -> size emptyQid

        (Rerror ename) -> size ename

        (Tflush oldtag) -> 2
        (Rflush)        -> 0

        (Tattach fid afid uname aname) -> 4 + 4 + size uname + size aname
        (Rattach qid) -> size emptyQid

        (Twalk fid newfid wname) -> 4 + 4 + 2 + size wname
        (Rwalk wqid) -> 2 + (length wqid)*(size emptyQid)

        (Topen fid mode) -> 4 + 1
        (Ropen qid iounit) -> size emptyQid + 4

        (Tcreate fid name perm mode) -> 4 + size name + 4 + 1
        (Rcreate qid iounit) -> size emptyQid + 4

        (Tread fid offset count) -> 4 + 8 + 4
        (Rread dat) -> 4 + size dat

        (Twrite fid offset dat) -> 4 + 8 + 4 + size dat
        (Rwrite count) -> 4

        (Tclunk fid) -> 4
        (Rclunk) -> 0

        (Tremove fid) -> 4
        (Rremove) -> 0

        (Tstat fid) -> 4
        (Rstat stat) -> size stat

        (Twstat fid stat) -> 4 + size stat
        (Rwstat) -> 0

