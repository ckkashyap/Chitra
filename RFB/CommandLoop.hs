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
	let commandData = RFBClient.parseCommandByteString byteStream command
	case command of
		0 -> processSetPixelFormat commandData
		2 -> processSetEncodings handle commandData 
		_ -> putStrLn "Some other command"
	
	commandLoop handle



processSetPixelFormat (bpp:
	depth:
	bigEndian:
	trueColor:
	redMax:
	greenMax:
	blueMax:
	redShift:
	greenShift:
	blueShift:[]) = do
		putStrLn "SetPixelFormat Command"
		putStrLn ("BPP = " ++ (show bpp))


processSetEncodings handle [count] = do
	putStrLn "SetEncodings Command"
	putStrLn (show count)
	x <- BS.hGet handle (4*count)
	return ()
