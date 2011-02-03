module RFB.Encoding (RFBState,initState,getImageByteString,setPixelFormat,putPixel) where

import qualified Data.ByteString.Lazy as BS
import Data.Binary.Get
import Data.Binary.Put
import Data.Word
import Data.Bits

type Red = Int
type Green = Int
type Blue = Int
type Color  = (Red,Green,Blue)

type ImageData = [Color]

--type BitsPerPixel = Int
type BigEndian = Int
type RedMax = Int
type GreenMax = Int
type BlueMax = Int
type RedShift = Int
type GreenShift = Int
type BlueShift = Int
data PixelFormat = PixelFormat BitsPerPixel BigEndian RedMax GreenMax BlueMax RedShift GreenShift BlueShift

data RFBState = RFBState ImageData PixelFormat


initState width height = RFBState imageData pixelFormat
	where
		imageData = replicate (width*height) (0,0,0)
		pixelFormat = PixelFormat (int2bpp 32) 1 255 255 255 0 8 16


setPixelFormat (RFBState imageData _) bpp bigEndian rm gm bm rs gs bs = RFBState imageData (PixelFormat (int2bpp bpp) bigEndian rm gm bm rs gs bs)

--getImageByteString :: RFBState -> BS.ByteString
getImageByteString (RFBState imageData (PixelFormat bpp _ _ _ _ _ _ _)) =  encodeImage imageData bpp


data BitsPerPixel = Bpp8 | Bpp16 | Bpp32 deriving (Show)


encodeImage [] _ = return ()
encodeImage ((r,g,b):xs) bpp = do
	encode (r,255,b) bpp
	encodeImage xs bpp


--int2bpp :: Int -> BitsPerPixel
int2bpp i
	| i == 8 = Bpp8
	| i == 16 = Bpp16
	| otherwise = Bpp32



encode (r,g,b) bitsPerPixel = do
	case bitsPerPixel of
		Bpp8	-> putWord8 z8
		Bpp16	-> putWord16le z16
		Bpp32	-> putWord32le z32
	where 
		z8  = (fromIntegral $ nr + ng + nb) :: Word8
		z16 = (fromIntegral $ nr + ng + nb) :: Word16
		z32 = (fromIntegral $ nr + ng + nb) :: Word32
		nr = scale r (r_max bitsPerPixel) (r_shift bitsPerPixel)
		ng = scale g (g_max bitsPerPixel) (g_shift bitsPerPixel)
		nb = scale b (b_max bitsPerPixel) (b_shift bitsPerPixel)
		scale c cm cs = (c * cm `div` 255) `shift` cs

r_max Bpp8 = 7
r_max Bpp16 = 31
r_max Bpp32 = 255

g_max Bpp8 = 7
g_max Bpp16 = 63
g_max Bpp32 = 255

b_max Bpp8 = 3
b_max Bpp16 = 31
b_max Bpp32 = 255

r_shift Bpp8 = 0
r_shift Bpp16 = 11
r_shift Bpp32 = 16

g_shift Bpp8 = 3
g_shift Bpp16 = 5
g_shift Bpp32 = 8

b_shift Bpp8 = 6
b_shift Bpp16 = 0
b_shift Bpp32 = 0


putPixel (RFBState imageData pf) (x,y) = RFBState newImgeData pf
	where
		newImgeData = (take count imageData) ++ [(255,0,0)] ++ (drop (count+1) imageData)
		count = width*y + x
		width = 100
