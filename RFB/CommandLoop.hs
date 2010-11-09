module RFB.CommandLoop (commandLoop) where

import System.IO
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Char
import Data.Binary.Get
import Data.Binary.Put
import Data.Word

import qualified RFB.ClientToServer as RFBClient

commandLoop :: Handle -> IO ()
commandLoop handle = do
	commandByte <- BS.hGet handle 1
	let command = runGet (do {x<-getWord8;return(x);}) commandByte
	byteStream <- BS.hGet handle (RFBClient.bytesToRead command)
	putStrLn (show command)
	putStrLn (show (RFBClient.bytesToRead command))
	let x = RFBClient.parseCommandByteString byteStream command
	putStrLn (show x)
	return ()


