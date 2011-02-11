{-# LANGUAGE NamedFieldPuns #-}
module RFB.CommandLoop (commandLoop) where

import System.IO
import qualified Data.ByteString.Lazy as BS

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
	let commandData = RFBClient.getCommandData byteStream command
	--let commandData = RFBClient.parseCommandByteString byteStream command
	liftIO $ putStrLn $ "Client said -> " ++ (show command)
	liftIO $ putStrLn (show commandData)
	case command of
		RFBClient.SetPixelFormat -> processSetPixelFormat commandData
		RFBClient.SetEncodings -> liftIO $ processSetEncodings handle commandData 
		RFBClient.FramebufferUpdate -> processFrameBufferUpdateRequest handle commandData
		RFBClient.PointerEvent -> processPointerEvent handle commandData
		RFBClient.ClientCutText -> processClientCutTextEvent handle commandData
		RFBClient.BadCommand -> liftIO $ putStrLn (show command)
	commandLoop1 handle



processClientCutTextEvent handle (RFBClient.ClientCutTextData length) = do
	x <- liftIO $ BS.hGet handle length
	liftIO $ putStrLn (show x)

	


processPointerEvent handle (RFBClient.PointerEventData buttonMask x y)= do
	state <- get
	let newState=if buttonMask == 1 then Encoding.putPixel state (x,y) else state
	put newState


processSetPixelFormat (RFBClient.SetPixelFormatData bitsPerPixel depth bigEndian trueColor redMax greenMax blueMax redShift greenShift blueShift)
	= do
		state <- get
		let newState=RFBState.setPixelFormat state bitsPerPixel bigEndian redMax greenMax blueMax redShift greenShift blueShift
		put newState
		return ()



processSetEncodings handle (RFBClient.SetEncodingsData count) = do
	x <- BS.hGet handle (4*count)
	let e = runGet (do {x<-getWord32le; return x}) x
	putStrLn (show e)
	return ()


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
	

processFrameBufferUpdateRequest handle (RFBClient.FramebufferUpdateData incremental x y width height) = do
	state <- get
	let byteString=(blueScreen x y width height state)
	liftIO $ BS.hPutStr handle byteString
	liftIO $ hFlush handle
	liftIO $ putStrLn ("Wrote " ++ (show $ BS.length byteString) ++ " bytes")

