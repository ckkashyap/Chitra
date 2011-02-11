module RFB.ClientToServer (
	Command
	(
		SetPixelFormat ,
		SetEncodings ,
		FramebufferUpdate ,
		KeyEvent ,
		PointerEvent ,
		ClientCutText ,
		BadCommand
	),
	CommandData
	(
		SetPixelFormatData ,
		SetEncodingsData ,
		FramebufferUpdateData ,
		KeyEventData ,
		PointerEventData ,
		ClientCutTextData
	),
	bytesToRead,
	parseCommandByteString,
	getCommandData,
	word8ToCommand,
) where

import Data.Word

import qualified Utilities.ParseByteString as PBS
import qualified Data.ByteString.Lazy as BS


data Command = SetPixelFormat | SetEncodings | FramebufferUpdate | KeyEvent | PointerEvent | ClientCutText | BadCommand deriving (Show)

data CommandData = SetPixelFormatData {
	bitsPerPixel	:: Int,
	depth		:: Int,
	bigEndian	:: Int,
	trueColor	:: Int,
	redMax		:: Int,
	greenMax	:: Int,
	blueMax		:: Int,
	redShift	:: Int,
	greenShift	:: Int,
	blueShift	:: Int
} | ClientCutTextData {
	length		:: Int
} | SetEncodingsData {
	count		:: Int
} | FramebufferUpdateData {
	incremental	:: Int,
	x		:: Int,
	y		:: Int,
	width		:: Int,
	height		:: Int
} | KeyEventData {
	downFlag	:: Int,
	key		:: Int
} | PointerEventData {
	buttonMask	:: Int,
	x		:: Int,
	y		:: Int
} | BadCommandData deriving (Show)



bytesToRead :: Command -> Int
bytesToRead c = foldr (+) 0 (map (\x -> if x == 0 then 1 else x) (commandFormat c))

commandFormat :: Command -> [Int] -- 0 for padding bytes
commandFormat c = case c of
		SetPixelFormat		-> [0,0,0,1,1,1,1,2,2,2,1,1,1,0,0,0]
		SetEncodings		-> [0,2]
		FramebufferUpdate	-> [1,2,2,2,2]
		KeyEvent		-> [1,0,0,4]
		PointerEvent		-> [1,2,2]
		ClientCutText		-> [0,0,0,4]
		BadCommand		-> []


parseCommandByteString :: BS.ByteString -> Command -> [Int]
parseCommandByteString byteString command =
	PBS.parseByteString list byteString
		where 
			list = (commandFormat command)

getCommandData	:: BS.ByteString -> Command -> CommandData
getCommandData byteString command = commandDataFromList list
	where
		list = PBS.parseByteString (commandFormat command) byteString
		commandDataFromList xs = case command of
			SetPixelFormat		-> SetPixelFormatData (xs!!0) (xs!!1) (xs!!2) (xs!!3) (xs!!4) (xs!!5) (xs!!6) (xs!!7) (xs!!8) (xs!!9)
			SetEncodings		-> SetEncodingsData (xs!!0)
			FramebufferUpdate	-> FramebufferUpdateData (xs!!0) (xs!!1) (xs!!2) (xs!!3) (xs!!4)
			KeyEvent		-> KeyEventData (xs!!0) (xs!!1)
			PointerEvent		-> PointerEventData (xs!!0) (xs!!1) (xs!!2)
			ClientCutText		-> ClientCutTextData (xs!!0)
			BadCommand		-> BadCommandData


word8ToCommand :: Word8 -> Command
word8ToCommand w = case w of
	0 -> SetPixelFormat
	2 -> SetEncodings
	3 -> FramebufferUpdate
	4 -> KeyEvent
	5 -> PointerEvent
	6 -> ClientCutText
	_ -> BadCommand
