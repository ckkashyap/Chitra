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
import qualified RFB.State as RFBState

import Control.Monad.State


type MyState a = StateT RFBState.RFBState  IO a

commandLoop :: Handle -> Int -> Int -> IO ()
commandLoop h width height= do
	runStateT (commandLoop1 h) (RFBState.initialState width height)
	return ()


commandLoop1 :: Handle -> MyState ()
commandLoop1 handle = do
	commandByte <- liftIO$ BS.hGet handle 1
	let command = RFBClient.word8ToCommand $ runGet (do {x<-getWord8;return(x);}) commandByte
	byteStream <- liftIO $ BS.hGet handle (RFBClient.bytesToRead command)
	let commandData = RFBClient.parseCommandByteString byteStream command
	case command of
		RFBClient.SetPixelFormat -> processSetPixelFormat commandData
		RFBClient.SetEncodings -> liftIO $ processSetEncodings handle commandData 
		RFBClient.FramebufferUpdate -> processFrameBufferUpdateRequest handle commandData
		RFBClient.PointerEvent -> processPointerEvent handle commandData
		RFBClient.ClientCutText -> processClientCutTextEvent handle commandData
		RFBClient.BadCommand -> liftIO $ putStrLn (show command)
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
		let newState=RFBState.setPixelFormat state bpp bigEndian redMax greenMax blueMax redShift greenShift blueShift
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

blueScreen :: Int -> Int -> Int -> Int -> RFBState.RFBState -> BS.ByteString
blueScreen x y width height state = runPut $ do
	putWord8 0
	putWord8 0
	putWord16be 1
	putWord16be (fromIntegral x)
	putWord16be (fromIntegral y)
	putWord16be (fromIntegral width)
	putWord16be (fromIntegral height)
	putWord32be 0
	Encoding.getImageByteString state x y width height state
	return ()
	

processFrameBufferUpdateRequest handle [inc, x,y,width,height] = do
	liftIO $ putStrLn ("FrameBufferUpdateRequest x=" ++ (show x) ++ ", y=" ++ (show y) ++ " width =" ++ (show width) ++ ", height=" ++ (show height) ++ ", inc=" ++ (show inc))
	state <- get
	let byteString=(blueScreen x y width height state)
	liftIO $ BS.hPutStr handle byteString
	liftIO $ hFlush handle
	liftIO $ putStrLn ("Wrote " ++ (show $ BS.length byteString) ++ " bytes")

