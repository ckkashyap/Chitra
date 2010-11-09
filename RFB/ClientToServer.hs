module RFB.ClientToServer where

import qualified Data.ByteString.Lazy as BS
import Control.Applicative ((<$>))
import Data.Word
import Data.Binary.Get


-- Client to Server messages
setPixelFormat                  = 0 :: Word8
setEncodings                    = 2 :: Word8
framebufferUpdateRequest        = 3 :: Word8
keyEvent                        = 4 :: Word8
pointerEvent                    = 5 :: Word8
clientCutText                   = 6 :: Word8


bytesToRead :: Word8 -> Int
bytesToRead c = 19
--bytesToRead c = length (commandFormat c) -- excluding the command byte



commandFormat :: Word8 -> [Int] -- 0 for padding bytes
commandFormat c
		| c == setPixelFormat	= [0,0,0,1,1,1,1,2,2,2,1,1,1,0,0,0]
		| c == setEncodings	= [0,2]
		| c == framebufferUpdateRequest = [0,2,2,2,2]
		| c == keyEvent 	= [1,2,4]
		| c == pointerEvent	= [1,2,2]
		| c == clientCutText	= [0,0,0,4]
		| otherwise		= []


data Action m = A1 (m Word8) | A2 (m Word16) | A4 (m Word32)

action 1 = A1 getWord8
action 2 = A2 getWord16be
action 4 = A4 getWord32be

newtype Id a = Id a

getAction :: Action Get -> Get (Action Id)
getAction (A1 act) = A1 . Id <$> act 
getAction (A2 act) = A2 . Id <$> act
getAction (A4 act) = A4 . Id <$> act

getActions :: [Action Get] -> Get [Action Id]
getActions  = mapM getAction

toVal (A1 (Id v)) = fromIntegral v :: Int
toVal (A2 (Id v)) = fromIntegral v :: Int
toVal (A4 (Id v)) = fromIntegral v :: Int

parseCommandByteString byteString command =
	zip list $ map toVal $ runGet (getActions actionList) byteString
		where 
			actionList = 
				map action (map dingo list)
			dingo x = if x == 0 then 1 else x
			list = (commandFormat command)

