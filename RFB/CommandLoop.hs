module RFB.CommandLoop (commandLoop) where

import System.IO
import qualified Data.ByteString.Lazy as BS
--import qualified Data.ByteString.Lazy.Char8 as B
import Data.Char
import Data.Binary.Get
import Data.Binary.Put
import Data.Word

import qualified RFB.ClientToServer as RFBClient
import qualified RFB.Encoding as Encoding

import Control.Monad.State


type MyState a = StateT Encoding.RFBState  IO a

commandLoop :: Handle -> Int -> Int -> IO ()
commandLoop h width height= do
	runStateT (commandLoop1 h) (Encoding.initState width height)
	return ()


commandLoop1 :: Handle -> MyState ()
commandLoop1 handle = do
	commandByte <- liftIO$ BS.hGet handle 1
	let command = runGet (do {x<-getWord8;return(x);}) commandByte
	byteStream <- liftIO $ BS.hGet handle (RFBClient.bytesToRead command)
	let commandData = RFBClient.parseCommandByteString byteStream command
	case command of
		0 -> processSetPixelFormat commandData
		2 -> liftIO $ processSetEncodings handle commandData 
		3 -> processFrameBufferUpdateRequest handle commandData
		5 -> processPointerEvent handle commandData
		6 -> processClientCutTextEvent handle commandData
		_ -> liftIO $ putStrLn ("Some other command" ++ (show (fromIntegral command)))
	
	commandLoop1 handle



processClientCutTextEvent handle [length] = do
	x <- liftIO $ BS.hGet handle length
	liftIO $ putStrLn (show x)

	


processPointerEvent handle [event,x,y] = do
	--liftIO $ putStrLn ("Mouse Pressed " ++ (show event) ++ "(" ++ (show x) ++ ", " ++ (show y) ++ ")")
	state <- get
	let newState=if event == 1 then Encoding.putPixel state (x,y) else state
	put newState


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
		state <- get
		let newState=Encoding.setPixelFormat state bpp bigEndian redMax greenMax blueMax redShift greenShift blueShift
		put newState
		liftIO $ putStrLn $ "SET PIXEL FORMAT called"
		liftIO $ putStrLn ( "bpp = " ++ (show bpp) ++ "\ndepth = " ++ (show depth) ++ "\nbig endian = " ++ (show bigEndian) ++ "\ntrueColor = " ++ (show trueColor) ++ "\nRED MAX = " ++ (show redMax) ++ "\nGREEN MAX = " ++ (show greenMax) ++ "\nblueMax = " ++ (show blueMax) ++ "\nred shift = " ++ (show redShift) ++ "\ngreen shift = " ++ (show greenShift) ++ "\nblue shift = " ++ (show blueShift) ++ "\n")
		return ()



processSetEncodings handle [count] = do
	putStrLn "SetEncodings Command"
	putStrLn (show count)
	x <- BS.hGet handle (4*count)
	let e = runGet (do {x<-getWord32le; return x}) x
	putStrLn (show e)
	return ()


xxx 0 = return ()
xxx n = do
	putWord8 0 -- blue
	putWord8 0 -- green
	putWord8 0 -- red
	putWord8 0   -- dummy
	xxx (n-1)

blueScreen :: Int -> Int -> Int -> Int -> Encoding.RFBState -> BS.ByteString
blueScreen x y width height state = runPut $ do
	putWord8 0
	putWord8 0
	putWord16be 1
	putWord16be (fromIntegral x)
	putWord16be (fromIntegral y)
	putWord16be (fromIntegral width)
	putWord16be (fromIntegral height)
	putWord32be 0
	Encoding.getImageByteString state
	return ()
	

processFrameBufferUpdateRequest handle [x,y,width,height] = do
	liftIO $ putStrLn ("FrameBufferUpdateRequest x=" ++ (show x) ++ ", y=" ++ (show y) ++ " width =" ++ (show width) ++ ", height=" ++ (show height))
	state <- get
	liftIO $ BS.hPutStr handle (blueScreen x y width height state)

