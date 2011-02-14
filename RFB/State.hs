module RFB.State (
	RFBState (RFBState),
	PixelFormat (PixelFormat),
	initialState,
	setPixelFormat,
	r_max,
	g_max,
	b_max,
	r_shift,
	g_shift,
	b_shift,
	BitsPerPixel( Bpp8, Bpp16, Bpp32),
	Rectangle (Rectangle)
	) where

type Red = Int
type Green = Int
type Blue = Int
type Color  = (Red,Green,Blue)

type ImageData = [Color]
type ImageDimension = (Int,Int)

type BigEndian = Int
type RedMax = Int
type GreenMax = Int
type BlueMax = Int
type RedShift = Int
type GreenShift = Int
type BlueShift = Int

data BitsPerPixel = Bpp8 | Bpp16 | Bpp32 deriving (Show)

data PixelFormat = PixelFormat BitsPerPixel BigEndian RedMax GreenMax BlueMax RedShift GreenShift BlueShift deriving (Show)

data Rectangle = Rectangle Int Int Int Int deriving (Show)

data RFBState = RFBState ImageDimension ImageData PixelFormat [Rectangle] deriving(Show)

initialState width height = RFBState (width,height) imageData pixelFormat [Rectangle 10 10 1 1]
	where
		imageData = replicate (width*height) (0,0,255)
		pixelFormat = PixelFormat Bpp32 1 255 255 255 16 8 0

setPixelFormat (RFBState dim imageData _ updateList) bpp bigEndian rm gm bm rs gs bs = RFBState dim imageData (PixelFormat (int2bpp bpp) bigEndian rm gm bm rs gs bs) updateList

int2bpp i
	| i == 8 = Bpp8
	| i == 16 = Bpp16
	| otherwise = Bpp32


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
