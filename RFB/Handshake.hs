module RFB.Handshake (handshake) where

import System.IO
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Char
import Data.Binary.Get
import Data.Binary.Put
import Data.Word

handshake :: Int -> Int -> Handle -> IO ()
handshake width height handle = do
	hPutStrLn handle "RFB 003.003"
	hFlush handle

	byteStream <- hGetLine handle
	putStrLn ("Client Said :: " ++ (show byteStream))
	let majorVersion = read $ (take 3).(drop 4) $ byteStream :: Int
	if majorVersion /= 3 then fail ("Expected 3 but the Client sent " ++ (show majorVersion))
		else return () -- contiue down the function

	-- Send 1 to the client, meaning, no auth required
	BS.hPutStr handle (BS.pack [0,0,0,1])
	hFlush handle

	clientInitMessage <- BS.hGet handle 1

	let sharedOrNot = runGet (do {x<-getWord8;return(x);}) clientInitMessage

	if sharedOrNot==1 then putStrLn "Sharing enabled"
		else putStrLn "Sharing disabled"

	BS.hPutStr handle (serverInitMessage width height)
	hFlush handle




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
				putWord8 (24::Word8) -- red shift
				putWord8 (1::Word8)  -- green shift
				putWord8 (1::Word8)  -- blue shift

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
