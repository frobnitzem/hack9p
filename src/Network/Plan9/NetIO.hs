{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Attoparsec as Atto
import qualified Data.ByteString.Char8 as S

import Control.Monad (void, forever)
import Control.Concurrent (forkIO, threadDelay)

handler :: NinePkt -> IO Builder
handler req = return $ handle req

handle :: NinePkt -> Builder
handle (Request "PING") = do
    copyByteString "PONG\r\n"

handle (Request _) = do
    copyByteString "ERROR\r\n"

main = testServer

testServer = do
    let definition = RequestPipeline parseRequest handler 10
    void $ forkIO $ runServer definition 6004
    forever $ threadDelay (1000000 * 60)

