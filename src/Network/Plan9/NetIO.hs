{-# LANGUAGE OverloadedStrings #-}
module Network.Plan9.NetIO where

import qualified Data.Attoparsec as Atto
import qualified Data.ByteString as S

import Control.Monad (void, forever)
import Control.Monad.Identity (Identity)
import Control.Concurrent (forkIO, threadDelay)

import Control.Wire

import Network.Plan9.Parser
import Network.Plan9.Builer

import Prelude hiding ((.), id)

srv :: Handle -> FileSysH IO -> Wire ServerError IO S.ByteString Builder
srv h fsh = (hGet9Msg h >>> arr decodeNinePktC >>> mkServer fsh >>> arr (mconcat . map buildNinePkt))

runNineServer :: FileSysH IO -> IO ()
runNineServer = do
	h <- hListen p9_port
	loop (srv h fsh) clockSession
    where
    loop w' session' = do
        (mx, w, session) <- stepSessionP w' session' ()
        case mx of
          Left ex -> do putStrLn ("Server Error: " ++ serr_name ex)
			serr_reset ex
          Right x -> hBuild h x
        loop w session

hGet9Msg h = mkFix $ \ _ _ -> do
	sz <- hReadWord32 h
	msg <- hReadByteString h sz
	return (Right msg)

parseRequest :: Atto.Parser (S.ByteString)
parseRequest = parseNinePkt 85532

{-
main = testServer

testServer = do
    let definition = RequestPipeline parseRequest handler 10
    void $ forkIO $ runServer definition p9_port
    forever $ threadDelay (1000000 * 60)
-}


import Data.ByteString.Char8
 
import Network hiding (accept)
import Network.Socket
import Network.Socket.ByteString (sendAll)
 
main = withSocketsDo $ do
    sock <- listenOn $ PortNumber 5002
    void $ forkIO $ loop sock
    forever $ threadDelay (1000000 * 60)
 
loop sock = do
   (conn, _) <- accept sock
   forkIO $ body conn
   loop sock
  where
   body c = do sendAll c msg
               sClose c
 
msg = "HTTP/1.0 200 OK\r\nContent-Length: 5\r\n\r\nPong!\r\n"
