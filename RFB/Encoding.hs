module RFB.Encoding (RFBState,initState,getImageByteString,setPixelFormat) where

import qualified Data.ByteString.Lazy as BS
import Data.Binary.Get
import Data.Binary.Put
import Data.Word

type Red = Int
type Green = Int
type Blue = Int
type Color  = (Red,Green,Blue)

type ImageData = [Color]

type BitsPerPixel = Int
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
		pixelFormat = PixelFormat 32 1 255 255 255 0 8 16


setPixelFormat (RFBState imageData _) bpp bigEndian rm gm bm rs gs bs = RFBState imageData (PixelFormat bpp bigEndian rm gm bm rs gs bs)

setPixelAtIndex imageData index color = before ++ [color] ++ after
	where
		before = take index imageData
		after = drop (index-1) imageData



--getImageByteString :: RFBState -> BS.ByteString
getImageByteString (RFBState imageData (PixelFormat bpp bigEndian rmax gmax bmax rshift gshift bshift)) = if bpp == 32 then buffer ((length imageData)*4) else buffer (length imageData)

buffer 0 = return ()
buffer n = do
	putWord8 255
	buffer (n-1)
