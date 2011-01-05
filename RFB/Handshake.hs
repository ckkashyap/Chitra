module RFB.Handshake (handshake) where

import System.IO
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Char
import Data.Binary.Get
import Data.Binary.Put
import Data.Word

import Control.Monad.State

type MyState a = StateT Int IO a



handshake :: Int -> Int -> Handle -> IO ()
handshake w h hnd =  do
	runStateT (handshake1 w h hnd) 0
	return ()
			

handshake1 :: Int -> Int -> Handle -> MyState ()
handshake1 width height handle = do
	liftIO $ hPutStrLn handle "RFB 003.003"
	liftIO $ hFlush handle

	byteStream <- liftIO $ hGetLine handle
	liftIO $ putStrLn ("Client Said :: " ++ (show byteStream))
	let majorVersion = read $ (take 3).(drop 4) $ byteStream :: Int
	if majorVersion /= 3 then liftIO (fail ("Expected 3 but the Client sent " ++ (show majorVersion)))
		else liftIO $ return () -- contiue down the function

	-- Send 1 to the client, meaning, no auth required
	liftIO $ BS.hPutStr handle (BS.pack [0,0,0,1])
	liftIO $ hFlush handle

	clientInitMessage <- liftIO $ BS.hGet handle 1

	let sharedOrNot = runGet (do {x<-getWord8;return(x);}) clientInitMessage

	if sharedOrNot==1 then liftIO $ putStrLn "Sharing enabled"
		else liftIO $ putStrLn "Sharing disabled"

	liftIO $ BS.hPutStr handle (serverInitMessage width height)
	liftIO $ hFlush handle




serverInitMessage :: Int -> Int -> BS.ByteString
serverInitMessage width height = runPut $ do
				putWord16be (fromIntegral width::Word16) -- width
				putWord16be (fromIntegral height::Word16) -- height
				--pixel format
				putWord8 (32::Word8) -- bits per pixl
				putWord8 (24::Word8) -- depth
				putWord8 (1::Word8) -- big endian
				putWord8 (1::Word8) -- true color
				putWord16be (255::Word16) -- red max
				putWord16be (255::Word16) -- green max
				putWord16be (255::Word16) -- blue max
				putWord8 (16::Word8) -- red shift
				putWord8 (8::Word8)  -- green shift
				putWord8 (0::Word8)  -- blue shift

				--padding
				putWord8 (0::Word8)
				putWord8 (0::Word8)
				putWord8 (0::Word8)

				--name length
				let name = "Haskell Framebuffer"
				putWord32be (((fromIntegral.length) name)::Word32)
				--mapM_ (putWord8.fromIntegral.ord) name --this works
				putLazyByteString (B.pack name)
				--putLazyByteString (stringToByteString name) --this works
