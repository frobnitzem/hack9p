{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Attoparsec as Atto
import qualified Data.ByteString as S

import Control.Monad (void, forever)
import Control.Concurrent (forkIO, threadDelay)

import Network.Plan9.NineP

handler :: S.ByteString -> IO Builder
handler s = let pkt = decodeNinePktC s
	    (mr, w) = stepWire wire 0 

handle (Request _) = do
    copyByteString "ERROR\r\n"

parseRequest :: Parser (S.ByteString)
parseRequest = parseNinePkt 85532

main = testServer

testServer = do
    let definition = RequestPipeline parseRequest handler 10
    void $ forkIO $ runServer definition 6004
    forever $ threadDelay (1000000 * 60)

