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
bytesToRead c = n - 1 where
	n
		| c == setPixelFormat	= 20
		| c == setEncodings	= 4
		| c == framebufferUpdateRequest = 10
		| c == keyEvent 	= 8
		| c == pointerEvent	= 6
		| c == clientCutText	= 8 -- more based on the last byte
