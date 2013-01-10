{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      : Network.Plan9.NineP
-- Copyright   : David M. Rogers and Tim Newsham
-- License     : GPL3 (see below)
-- 
-- Maintainer  : David M. Rogers <predictivestatmech@gmail.com>
-- Stability   : experimental
-- Portability : Only tested on GHC 7.4.1, uses TypeSynonymInstances
--
-- Module providing Binary serialization of 9P messages to and from lazy 
-- ByteStrings.
-- 
-- 
-- 9P2000 messages are sent in little endian byte order rather than network byte order 
-- (big endian)
--
-----------------------------------------------------------------------------
--
{-
 - Copyright (c) 2013  David M. Rogers <predictivestatmech@gmail.com>
 -  
 -  This file is part of hack9p.
 -
 -  hack9p is free software: you can redistribute it and/or modify
 -  it under the terms of the GNU General Public License as published by
 -  the Free Software Foundation, either version 3 of the License, or
 -  (at your option) any later version.
 -
 -  hack9p is distributed in the hope that it will be useful,
 -  but WITHOUT ANY WARRANTY; without even the implied warranty of
 -  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 -  GNU General Public License for more details.
 -
 -  You should have received a copy of the GNU General Public License
 -  along with hack9p.  If not, see <http://www.gnu.org/licenses/>.
 -
 - This file incorporates work covered by the following copyright and  
 - permission notice:  
 -  
 -    Copyright (c) 2010, Tim Newsham and David Leimbach
 -    All rights reserved.
 -    
 -    Redistribution and use in source and binary forms, with or without
 -    modification, are permitted provided that the following conditions are met:
 -    
 -    1. Redistributions of source code must retain the above copyright notice,
 -    this list of conditions and the following disclaimer.
 -    
 -    2. Redistributions in binary form must reproduce the above copyright
 -    notice, this list of conditions and the following disclaimer in the
 -    documentation and/or other materials provided with the distribution.
 -    
 -    3. Neither the names of the authors nor the names of other project
 -    contributors may be used to endorse or promote products derived from
 -    this software without specific prior written permission.
 -
 -    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 -    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 -    TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
 -    PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR
 -    CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 -    EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 -    PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 -    PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 -    LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 -    NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 -    SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 -}

module Network.Plan9.NineP ( 
                  -- * Bin - a little endian encode/decode class for Bin values
                   Bin(..)
                  -- * Qid - Server side data type for path tracking (<http://9p.cat-v.org> for details)
                  , Qid(..)
                  , emptyQid
                  -- * Stat - Namespace metadata /(somewhat like a unix fstat)/
                  , Stat(..)
                  , emptyStat

                  -- ** MsgTyp - A message payload type
                  , MsgTyp(..)
                  , typInt

                  -- ** NineMsg - An envelope encapsulating the various 9P2000 messages 
                  , NineMsg(..)
                          
                  -- ** putNineMsg - function that can encode all NineMsg types to a lazy ByteString
                  , putNineMsg, putNinePkt

                  -- ** getNineMsg - function to decode all NineMsg types from a lazy ByteString
                  , getNineMsg, decodeNinePkt, getNinePkt

                  -- * Example
                  -- $example
                  ) where

import Control.Applicative
import Control.Monad
import Data.Binary.Get
import Data.Binary.Put
--import Data.Char
import Data.Word

import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L

--import Data.Monoid((<>), mconcat)

import Data.Text(Text)
--import qualified Data.Text as T
import Data.Text.Encoding(encodeUtf8, decodeUtf8)

{-
import Debug.Trace 
tr msg n = trace (msg ++ show n) n
-- -}

-- Little-endian!
class Bin a where
    get :: Get a
    put :: a -> Put

instance Bin Word8 where
    get = getWord8
    put = putWord8

instance Bin Word16 where
    get = getWord16le
    put = putWord16le

instance Bin Word32 where
    get = getWord32le
    put = putWord32le

instance Bin Word64 where
    get = getWord64le
    put = putWord64le

{-
instance Bin Char where
    get = chr . fromIntegral <$> getWord8
    put = putWord8 . fromIntegral . ord
-}

instance Bin Text where
    get = getWord16le >>= \ sz -> decodeUtf8 <$> getBytes (fromIntegral sz)
    put xs = let bs = encodeUtf8 xs
             in do putWord16le (fromIntegral $ S.length bs)
	           putByteString bs

-- | A Plan 9 Qid type.  See http://9p.cat-v.org for more information
data Qid = Qid {
    qid_typ :: Word8, -- qid.type[1] the type of the file (directory, etc.),
                      -- represented as a bit vector corresponding to the high
                      --   8 bits of the file's mode word. 
    qid_vers :: Word32, -- qid.vers[4] version number for given path 
    qid_path :: Word64 -- qid.path[8] the file server's unique identification
		       --   for the file 
} deriving (Eq, Ord, Show)
emptyQid :: Qid
emptyQid = Qid {
    qid_typ = 0,
    qid_vers = 0,
    qid_path = 0
}
-- Instance of Bin for Qid.
instance Bin Qid where
    get = Qid <$> get <*> get <*> get
    put (Qid t v p) = put t >> put v >> put p

{- | Provides information on a path entry at a 9P2000 server
-}

--          size[2] (not included)
--              total byte count of the following data 
data Stat = Stat {
    st_typ :: Word16, -- type[2] for kernel use 
    st_dev :: Word32, -- dev[4] for kernel use 
    st_qid :: Qid, -- @Qid@
    st_mode :: Word32, -- mode[4] permissions and flags 
    st_atime :: Word32, -- atime[4] last access time 
    st_mtime :: Word32, -- mtime[4] last modification time 
    st_length :: Word64, -- length[8] length of file in bytes 
    st_name :: Text, -- name[ s ] file name; must be / if the file is the root directory of the server 
    st_uid :: Text, -- uid[ s ] owner name 
    st_gid :: Text, -- gid[ s ] group name 
    st_muid :: Text -- muid[ s ] name of the user who last modified the file 
-- Extensions:
{-  For dot u extensions:
    There are four new fields in the stat structure supporting 9P2000 extensions - as well as new qid.type bits and mode bits.
    The n_uid, n_gid, and n_muid are numeric hints that clients may use to map
  numeric ids when a string to numeric id mapping facility is not available.
    extension[s] is a string describing special files, depending on the mode
  bit. For DSYMLINK files, the string is the target of the link. For DMDEVICE
  files, the string is "b 1 2" for a block device with major 1, minor 2.
  For normal files, this string is empty.
-}
{-
    st_extension :: Text,-- extension[ s ] For use by the UNIX extension to store data about special files (links, devices, pipes, etc.) 
    st_n_uid :: Word32, -- n_uid[4] numeric id of the user who owns the file 
    st_n_gid :: Word32, -- n_gid[4] numeric id of the group associated with the file 
    st_n_muid :: Word32 -- n_muid[4] numeric id of the user who last modified the file 
-}
} deriving (Eq, Ord, Show)
emptyStat :: Stat
emptyStat = Stat {
    st_typ = 0,
    st_dev = 0,
    st_qid = emptyQid,
    st_mode = 0,
    st_atime = 0,
    st_mtime = 0,
    st_length = 0,
    st_name = "",
    st_uid = "",
    st_gid = "",
    st_muid = ""
 {- .u extensions:
    st_extension = "",
    st_n_uid = 0,
    st_n_gid = 0,
    st_n_muid = 0 -}
}
instance Bin Stat where
    get = do
        n <- getWord16le
        getNest n g
      where g = Stat <$> get <*> get <*> get <*> get <*> get <*> get <*> get <*> get <*> get <*> get <*> get
    put (Stat a b c d e f g h i j k) = do
        let buf = runPut p
        putWord16le $ fromIntegral $ L.length buf
        putLazyByteString buf
      where p  = put a >> put b >> put c >> put d >> put e >> put f >> put g >> put h >> put i >> put j >> put k


-- | A variable message type that encapsulates the valid kinds of messages in a 9P2000 payload
-- see http://man.cat-v.org/plan_9/5/intro
-- and http://ericvh.github.com/9p-rfc/rfc9p2000.u.html
data NineMsg = 
    Tversion { tv_msize :: Word32,
               tv_version :: Text
    }
    | Rversion { rv_msize :: Word32,
                 rv_version :: Text
    }
    | Tauth { tau_afid :: Word32,
              tau_uname :: Text,
              tau_aname :: Text
    }
    | Rauth { ra_aqid :: Qid }
    | Rerror { re_ename :: Text
	      -- re_errno :: Word32 -- 9p2000.u extension
    }
    | Tflush { tf_oldtag :: Word16 }
    | Rflush 
    | Tattach { tat_fid :: Word32,
	        tat_afid :: Word32,
		tat_uname :: Text,
		tat_aname :: Text
    }
    | Rattach { rat_qid :: Qid }
    | Twalk { tw_fid :: Word32,
              tw_newfid :: Word32,
	      tw_wnames :: [Text]
    }
    | Rwalk { rw_wqid :: [Qid] }
    | Topen { to_fid :: Word32,
	      to_mode :: Word8
    }
    | Ropen { ro_qid :: Qid,
	      ro_iounit :: Word32
    }
    | Tcreate { tcr_fid :: Word32,
	        tcr_name :: Text,
		tcr_perm :: Word32,
		tcr_mode :: Word8
    }
    | Rcreate { rcr_qid :: Qid,
	        rcr_iounit :: Word32
    }
    | Tread { trd_fid :: Word32,
	      trd_offset :: Word64,
	      trd_count :: Word32
    }
    | Rread { rrd_dat :: L.ByteString }
    | Twrite { twr_fid :: Word32,
	       twr_offset :: Word64,
	       twr_dat :: L.ByteString
    }
    | Rwrite { rw_count :: Word32 }
    | Tclunk { tcl_fid :: Word32 }
    | Rclunk
    | Tremove { trm_fid :: Word32 }
    | Rremove
    | Tstat { ts_fid :: Word32 }
    | Rstat { rs_stat :: [Stat] }
    | Twstat { tws_fid :: Word32,
	       tws_stat :: [Stat]
    }
    | Rwstat
    deriving (Show, Eq)

-- | A type that enumerates all the valid /(and one invalid)/ message types in 9P2000
data MsgTyp = TTversion | TRversion | TTauth | TRauth | TTattach | TRattach
    | TFail | TRerror | TTflush | TRflush
    | TTwalk | TRwalk | TTopen | TRopen
    | TTcreate | TRcreate | TTread | TRread | TTwrite | TRwrite
    | TTclunk | TRclunk | TTremove | TRremove | TTstat | TRstat
    | TTwstat | TRwstat
    deriving (Show, Eq, Ord, Enum)

instance Bin MsgTyp where
    get = typName <$> getWord8
    put = putWord8 . fromIntegral . typInt

-- | Parse a @ByteString@ containing the @NineMsg@ payload (all except the leading sz.)
--   into a tag and a message body.
decodeNinePkt :: L.ByteString -> (Int, NineMsg)
decodeNinePkt = runGet getNinePkt

getNinePkt :: Get (Int, NineMsg)
getNinePkt = do
    typ <- typName <$> getWord8
    tag <- fromIntegral <$> getWord16le
    (\ x -> (tag, x)) <$> getNineMsg typ

-- | Takes a fixed size lazy ByteString @sz@, running @g@ on it, and returning the result if all the input is consumed, uses @Prelude.error@ otherwise. (Throws an exception)
getNest :: Integral n => n -> Get a -> Get a
getNest sz g = do
    b <- getLazyByteString (fromIntegral sz)
    return $ flip runGet b $ do
        x <- g
        e <- isEmpty
        if e
          then return x
          else do
              n <- remaining
              error $ show n ++ " extra bytes in nested structure"

-- | A monadic action in Get that returns all parseable entries as a list, returning [] if none exist
getListAll :: (Bin a) => Get [a]
getListAll = do
    e <- isEmpty 
    if e 
      then return [] 
      else (:) <$> get <*> getListAll

-- | A monadic action in Put that maps all encodable items into the Put monad for serialization
putListAll :: (Bin a) => [a] -> Put
putListAll = mapM_ put

-- | Like 'getNest' but is preceded by a little endian 16bit word.  Useful for retrieving 16bit payload lengths from 9P2000 messages that support it.
getNestList16 :: (Bin a) => Get [a]
getNestList16 = do
    n <- getWord16le
    getNest n getListAll

-- | Runs 'putListAll' over @xs@ followed by computing the 16bit (little endian) length of the list, and prepending it to the final payload.  Useful for automatically computing lengths for 9P2000 messages that require it
putNestList16 :: Bin a => [a] -> Put
putNestList16 xs = do
    let buf = runPut (putListAll xs)
    putWord16le $ fromIntegral $ L.length buf
    putLazyByteString buf

-- | Gets a 16bit value from the stream, then executes @Data.Binary.Get.get@ that many times to produce a list of parsed values.
getList16 :: Bin a => Get [a]
getList16 = getWord16le >>= \n -> replicateM (fromIntegral n) get

-- | Puts @xs@ into the Put stream prepended by its length.
putList16 :: Bin a => [a] -> Put
putList16 xs = putWord16le (fromIntegral $ length xs) >> mapM_ put xs

-- | Gets a 32bit little endian legnth from the stream, then gets a lazy ByteString of that size from the stream, returning only the ByteString
getBytes32 :: Get L.ByteString
getBytes32 = getWord32le >>= getLazyByteString . fromIntegral

-- | Takes length of @xs@ and then prepends that to the lazy ByteString it places in the Stream
putBytes32 :: L.ByteString -> Put
putBytes32 xs = putWord32le (fromIntegral $ L.length xs) >> putLazyByteString xs

-- | For a given message type, produces a Get parser to decode that type of payload from the 9P2000 stream
getNineMsg :: MsgTyp -> Get NineMsg
getNineMsg typ = case typ of
	TTversion -> Tversion <$> get <*> get
	TRversion -> Rversion <$> get <*> get
	TTauth -> Tauth <$> get <*> get <*> get
	TRauth -> Rauth <$> get
	--TFail  -> Fail "Invalid message type."
	TRerror -> Rerror <$> get
	TTflush -> Tflush <$> get
	TRflush -> return Rflush
	TTattach -> Tattach <$> get <*> get <*> get <*> get
	TRattach -> Rattach <$> get
	TTwalk -> Twalk <$> get <*> get <*> getList16
	TRwalk -> Rwalk <$> getList16
	TTopen -> Topen <$> get <*> get
	TRopen -> Ropen <$> get <*> get
	TTcreate -> Tcreate <$> get <*> get <*> get <*> get
	TRcreate -> Rcreate <$> get <*> get
	TTread -> Tread <$> get <*> get <*> get
	TRread -> Rread <$> getBytes32
	TTwrite -> Twrite <$> get <*> get <*> getBytes32
	TRwrite -> Rwrite <$> get
	TTclunk -> Tclunk <$> get
	TRclunk -> return Rclunk
	TTremove -> Tremove <$> get
	TRremove -> return Rremove
	TTstat -> Tstat <$> get
	TRstat -> Rstat <$> getNestList16
	TTwstat -> Twstat <$> get <*> getNestList16
	TRwstat -> return Rwstat

-- | For every lower level NineMsg type, encodes a full wrapper around that type for use with 9P2000 streams
putNineMsg :: NineMsg -> Put
putNineMsg msg = case msg of
	(Tversion a b) -> put a >> put b
	(Rversion a b) -> put a >> put b
	(Tauth a b c) -> put a >> put b >> put c
	(Rauth a) -> put a
	(Rerror a) -> put a
	(Tflush a) -> put a
	(Rflush) -> return ()
	(Tattach a b c d) -> put a >> put b >> put c >> put d
	(Rattach a) -> put a
	(Twalk a b c) -> put a >> put b >> putList16 c
	(Rwalk a) -> putList16 a
	(Topen a b) -> put a >> put b
	(Ropen a b) -> put a >> put b
	(Tcreate a b c d) -> put a >> put b >> put c >> put d
	(Rcreate a b) -> put a >> put b
	(Tread a b c) -> put a >> put b >> put c
	(Rread a) -> putBytes32 a
	(Twrite a b c) -> put a >> put b >> putBytes32 c
	(Rwrite a) -> put a
	(Tclunk a) -> put a
	(Rclunk) -> return ()
	(Tremove a) -> put a
	(Rremove) -> return ()
	(Tstat a) -> put a
	(Rstat a) -> putNestList16 a
	(Twstat a b) -> put a >> putNestList16 b
	(Rwstat) -> return ()

putNinePkt :: (Integral a) => (a, NineMsg) -> PutM ()
putNinePkt (tag, body) = do
        let typ = msgTyp body
            buf = runPut (put typ >> (putWord16le . fromIntegral) tag >> putNineMsg body)
        putWord32le $ fromIntegral $ L.length buf + 4
        putLazyByteString buf

typName :: Word8 -> MsgTyp
typName n = if n >= 100 && n < 128 && n /= 106 -- 106 == Terror (Fail)
                    then toEnum $ fromEnum (n-100)
                    else TFail
typInt :: MsgTyp -> Int
typInt = (+ 100) . fromEnum

-- | Converts NineMsg types to Tag values
msgTyp :: NineMsg -> MsgTyp
msgTyp msg = case msg of
	(Tversion _ _) -> TTversion
	(Rversion _ _) -> TRversion
	(Tauth _ _ _) -> TTauth
	(Rauth _) -> TRauth
	(Tflush _) -> TTflush
	(Rflush) -> TRflush
	(Tattach _ _ _ _) -> TTattach
	(Rattach _) -> TRattach
	(Rerror _) -> TRerror
	(Twalk _ _ _) -> TTwalk
	(Rwalk _) -> TRwalk
	(Topen _ _) -> TTopen
	(Ropen _ _) -> TRopen
	(Tcreate _ _ _ _) -> TTcreate
	(Rcreate _ _) -> TRcreate
	(Tread _ _ _) -> TTread
	(Rread _) -> TRread
	(Twrite _ _ _) -> TTwrite
	(Rwrite _) -> TRwrite
	(Tclunk _) -> TTclunk
	(Rclunk) -> TRclunk
	(Tremove _) -> TTremove
	(Rremove) -> TRremove
	(Tstat _) -> TTstat
	(Rstat _) -> TRstat
	(Twstat _ _) -> TTwstat
	(Rwstat) -> TRwstat

-- --------------------------------------------------------------------
-- $example
--
-- Exchanging initial version data with any 9P2000 server
--
-- > module Main where
--
-- > import Data.Maybe
-- > import Control.Monad
-- > import qualified Data.ByteString.Lazy.Char8 as C
-- > import Network.Socket hiding (send, recv)
-- > import Network.Socket.ByteString.Lazy
-- > import Data.Int
-- > import Data.Binary.Get
-- > import Data.Binary.Put
-- > import Debug.Trace
-- > import Data.NineP
-- > 
-- > connector :: IO Socket 
-- > connector = withSocketsDo $
-- >             do
-- >               ainfo <- getAddrInfo Nothing (Just "127.0.0.1") (Just "6872")
-- >               let a = head ainfo
-- >               sock <- socket AF_INET Stream defaultProtocol
--
-- At this point we've just created our socket to a machine on 127.0.0.1:6872 
-- where we'd expect to see a 9P2000 server.
--
-- >               putStrLn "Trying to connect"
-- >               connect sock (addrAddress (traceShow a a))
-- >               putStrLn "connected!"
--
-- The socket is connected at this point, build up a TVersion message, asking
-- to speak to the server with the 9P2000 protocol. 
--
-- The 1024 tells the server the maximum message size we'd like to support.
--
-- >               let version = NinePkt TTversion (-1) $ Tversion 1024 "9P2000"
-- >               putStrLn $ "About to send: " ++ show version
--
-- We now need to pack the message into a bytestring.  This is handled by the
-- Bin class instance /NinePkt/, and the serialization is handled by runPut.
-- We send this data to the socket.
--
-- >               send sock $ runPut (put version) 
-- >               putStrLn "Getting response"
--
-- Now wait for a response from the server, evaluated runGet over it to 
-- de-serialize it, and show it.
--
-- >               msg <- recv sock 50
-- >               let response = runGet get msg :: NinePkt
-- >               putStrLn $ show response
-- >               return sock
