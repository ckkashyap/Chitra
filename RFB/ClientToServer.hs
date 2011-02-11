module RFB.ClientToServer (
	ClientCommand
	(
		SetPixelFormat ,
		SetEncodings ,
		FramebufferUpdate ,
		KeyEvent ,
		PointerEvent ,
		ClientCutText ,
		BadCommand
	),
	bytesToRead,
	parseCommandByteString,
	word8ToCommand
) where

import Data.Word

import qualified Utilities.ParseByteString as PBS
import qualified Data.ByteString.Lazy as BS


data ClientCommand = SetPixelFormat | SetEncodings | FramebufferUpdate | KeyEvent | PointerEvent | ClientCutText | BadCommand deriving (Show)

bytesToRead :: ClientCommand -> Int
bytesToRead c = foldr (+) 0 (map (\x -> if x == 0 then 1 else x) (commandFormat c))

commandFormat :: ClientCommand -> [Int] -- 0 for padding bytes
commandFormat c = case c of
		SetPixelFormat		-> [0,0,0,1,1,1,1,2,2,2,1,1,1,0,0,0]
		SetEncodings		-> [0,2]
		FramebufferUpdate	-> [1,2,2,2,2]
		KeyEvent		-> [1,2,4]
		PointerEvent		-> [1,2,2]
		ClientCutText		-> [0,0,0,4]
		BadCommand		-> []


parseCommandByteString :: BS.ByteString -> ClientCommand -> [Int]
parseCommandByteString byteString command =
	PBS.parseByteString list byteString
		where 
			list = (commandFormat command)


word8ToCommand :: Word8 -> ClientCommand
word8ToCommand w = case w of
	0 -> SetPixelFormat
	2 -> SetEncodings
	3 -> FramebufferUpdate
	4 -> KeyEvent
	5 -> PointerEvent
	6 -> ClientCutText
	_ -> BadCommand
