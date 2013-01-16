{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      : Network.Plan9.Parser
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

module Network.Plan9.Parser ( 
                -- * Bin - a little endian encode/decode class for Bin values
                 Bin(..)

                -- ** encodeNinePkt - function that can encode all NinePkt types to a lazy ByteString
                --    Note: The builders in 
                , encodeNinePkt
		, putNinePkt

                -- * parseNineMsg - Attoparsec function to parse the 4-byte message size
                -- and return the packet as a bytestring.
                , parseNinePkt

                -- ** decodeNinePkt - function to decode all NinePkt types from a lazy ByteString
                -- The 'C' versions expect the leading 4-byte message size to be gone
                , decodeNinePkt, decodeNinePktC
		, getNinePktC
		, typInt
		, typName
		, msgTyp
                -- * Example
                -- $example
                ) where

import Control.Applicative
import Control.Monad
import Data.Binary.Get
import Data.Binary.Put
import Data.Word

import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L

import Data.Attoparsec ((<?>), Parser)
import Data.Attoparsec.Binary (anyWord32le)
import qualified Data.Attoparsec as Atto

import Data.Text(Text)
import Data.Text.Encoding(encodeUtf8, decodeUtf8)

import Network.Plan9.Consts

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

instance Bin Text where
    get = getWord16le >>= \ sz -> decodeUtf8 <$> getBytes (fromIntegral sz)
    put xs = let bs = encodeUtf8 xs
             in do putWord16le (fromIntegral $ S.length bs)
	           putByteString bs

-- Instance of Bin for Qid.
instance Bin Qid where
    get = Qid <$> get <*> get <*> get
    put (Qid t v p) = put t >> put v >> put p

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

instance Bin MsgTyp where
    get = typName <$> getWord8
    put = putWord8 . fromIntegral . typInt

-- | Parse a @ByteString@ containing a 9P2000 format message
--  into a tag and a message body.
decodeNinePkt :: L.ByteString -> (Word16, NineMsg)
decodeNinePkt = runGet (getWord32le >> getNinePktC)

-- | A Parser that reads the 4-byte length and returns
-- a @ByteString@ with the rest of the message.
-- Suitable for decoding with @decodeNinePktC@.
parseNinePkt :: Int  -> Parser (S.ByteString)
parseNinePkt maxSize = (<?> "NinePkt") $ do
  sz <- fromIntegral <$> anyWord32le
  if sz < 4+1+2 || sz > maxSize
    then fail $ "Invalid NinePkt size: " ++ (show sz)
    else Atto.take (sz-4)

-- | Parse a @ByteString@ containing a 9P2000 format message (sans the
--  leading length field) into a tag and a message body.
decodeNinePktC :: L.ByteString -> (Word16, NineMsg)
decodeNinePktC = runGet getNinePktC

-- | A @Data.Binary.Get@ parser for 9P2000 format messages, which 
-- do not include a leading length.  This returns the tag and message body.
getNinePktC :: Get (Word16, NineMsg)
getNinePktC = do
    typ <- typName <$> getWord8
    tag <- getWord16le
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
	TFail  -> return $ Rerror "[@yourstack] Bad parse." -- Fail "Invalid message type."
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
	TRstat -> Rstat <$> get
	TTwstat -> Twstat <$> get <*> get
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
	(Rstat a) -> put a
	(Twstat a b) -> put a >> put b
	(Rwstat) -> return ()

-- | Encode a tag and message body into a @ByteString@ conforming to 9P2000.
encodeNinePkt :: (Word16, NineMsg) -> L.ByteString
encodeNinePkt = runPut . putNinePkt

-- | Create a Put monad for serializing a tag and message body into a
-- @ByteString@ conforming to 9P2000.
putNinePkt :: (Word16, NineMsg) -> PutM ()
putNinePkt (tag, body) = do
        let typ = msgTyp body
            buf = runPut (put typ >> put tag >> putNineMsg body)
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
