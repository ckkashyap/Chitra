module RFB.ClientToServer where

import Data.Word

-- Client to Server messages
setPixelFormat                  = 0 :: Word8
setEncodings                    = 2 :: Word8
framebufferUpdateRequest        = 3 :: Word8
keyEvent                        = 4 :: Word8
pointerEvent                    = 5 :: Word8
clientCutText                   = 6 :: Word8


bytesToRead :: Word8 -> Int
bytesToRead c = length (commandFormat c) -- excluding the command byte



commandFormat :: Word8 -> [Int] -- 0 for padding bytes
commandFormat c
		| c == setPixelFormat	= [0,0,0,1,1,1,1,2,2,2,1,1,1,0,0,0]
		| c == setEncodings	= [0,2]
		| c == framebufferUpdateRequest = [0,2,2,2,2]
		| c == keyEvent 	= [1,2,4]
		| c == pointerEvent	= [1,2,2]
		| c == clientCutText	= [0,0,0,4]
		| otherwise		= []


--parseCommandByteString byteString command = 

