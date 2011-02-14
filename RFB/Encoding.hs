module RFB.Encoding (getImageByteString,putPixel,encodeRectagles) where

import qualified Data.ByteString.Lazy as BS
import Data.Binary.Get
import Data.Binary.Put
import Data.Word
import Data.Bits

import qualified RFB.State as RFBState


getImageByteString (RFBState.RFBState (totalWidth,_) imageData (RFBState.PixelFormat bpp _ _ _ _ _ _ _) updateList) x y width height =  encodeImage (clip imageData x y width height totalWidth) bpp




clip imageData x y width height totalWidth = horizontolClip verticalClip
	where
		verticalClip = take (height*totalWidth) (drop (y*totalWidth) imageData)
		horizontolClip [] = []
		horizontolClip image = (take width (drop x image)) ++ (horizontolClip (drop totalWidth image))



encodeImage [] _ = return ()
encodeImage ((r,g,b):xs) bpp = do
	encode (r,g,b) bpp
	encodeImage xs bpp



encode (r,g,b) bitsPerPixel = do
	case bitsPerPixel of
		RFBState.Bpp8	-> putWord8 z8
		RFBState.Bpp16	-> putWord16le z16
		RFBState.Bpp32	-> putWord32le z32
	where 
		z8  = (fromIntegral $ nr + ng + nb) :: Word8
		z16 = (fromIntegral $ nr + ng + nb) :: Word16
		z32 = (fromIntegral $ nr + ng + nb) :: Word32
		nr = scale r (RFBState.r_max bitsPerPixel) (RFBState.r_shift bitsPerPixel)
		ng = scale g (RFBState.g_max bitsPerPixel) (RFBState.g_shift bitsPerPixel)
		nb = scale b (RFBState.b_max bitsPerPixel) (RFBState.b_shift bitsPerPixel)
		scale c cm cs = (c * cm `div` 255) `shift` cs


encodeRectagles _ []  = return ()
encodeRectagles bpp ((RFBState.Rectangle x y w h):xs) = do
	putWord16be (fromIntegral x)
	putWord16be (fromIntegral y)
	putWord16be (fromIntegral w)
	putWord16be (fromIntegral h)
	putWord32be 0
	encode (r,g,b) bpp
	encodeRectagles bpp xs
		where
			r = 255 :: Word8
			g = 255 :: Word8
			b = 255 :: Word8



putPixel (RFBState.RFBState dim@(width,_) imageData pf updateList) (x,y) = RFBState.RFBState dim newImgeData pf ((RFBState.Rectangle x y 1 1):updateList)
	where
		newImgeData = (take count imageData) ++ [(255,255,255)] ++ (drop (count+1) imageData)
		count = width*y + x

