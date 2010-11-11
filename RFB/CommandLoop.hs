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
		3 -> processFrameBufferUpdateRequest handle commandData
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


blueScreen :: BS.ByteString
blueScreen = runPut $ do
	putWord8 0
	putWord8 0
	putWord16be 1
	putWord16be 0
	putWord16be 0
	putWord16be 100
	putWord16be 100
	putWord32be 0
	

processFrameBufferUpdateRequest handle [x,y,width,height] = do
	putStrLn ("FrameBufferUpdateRequest x=" ++ (show x) ++ ", y=" ++ (show y) ++ " width =" ++ (show width) ++ ", height=" ++ (show height))
	BS.hPutStr handle blueScreen

