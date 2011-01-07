module RFB.CommandLoop (commandLoop) where

import System.IO
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Char
import Data.Binary.Get
import Data.Binary.Put
import Data.Word

import qualified RFB.ClientToServer as RFBClient

import Control.Monad.State

type MyState a = StateT Int IO a

commandLoop :: Handle -> IO ()
commandLoop h = do
	runStateT (commandLoop1 h) 0
	return ()


commandLoop1 :: Handle -> MyState ()
commandLoop1 handle = do
	commandByte <- liftIO$ BS.hGet handle 1
	let command = runGet (do {x<-getWord8;return(x);}) commandByte
	byteStream <- liftIO $ BS.hGet handle (RFBClient.bytesToRead command)
	let commandData = RFBClient.parseCommandByteString byteStream command
	case command of
		0 -> liftIO $ processSetPixelFormat commandData
		2 -> liftIO $ processSetEncodings handle commandData 
		3 -> processFrameBufferUpdateRequest handle commandData
		5 -> processPointerEvent handle commandData
		_ -> liftIO $ putStrLn "Some other command"
	
	commandLoop1 handle



processPointerEvent handle [event,x,y] = do
	n <- get
	put (255-n)
	liftIO $ putStrLn ("Mouse Pressed" ++ (show n) ++ "\n")

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


xxx 0 _= return ()
xxx n c= do
	putWord8 c -- blue
	putWord8 c -- green
	putWord8 c -- red
	putWord8 0   -- dummy
	xxx (n-1) c

blueScreen :: Int -> Int -> Int -> Int -> Word8 -> BS.ByteString
blueScreen x y width height n = runPut $ do
	putWord8 0
	putWord8 0
	putWord16be 1
	putWord16be (fromIntegral x)
	putWord16be (fromIntegral y)
	putWord16be (fromIntegral width)
	putWord16be (fromIntegral height)
	putWord32be 0
	xxx (width*height) n
	

processFrameBufferUpdateRequest handle [x,y,width,height] = do
	--liftIO $ putStrLn ("FrameBufferUpdateRequest x=" ++ (show x) ++ ", y=" ++ (show y) ++ " width =" ++ (show width) ++ ", height=" ++ (show height))
	n <- get
	--put (255-n)
	liftIO $ BS.hPutStr handle (blueScreen x y width height (fromIntegral n))

