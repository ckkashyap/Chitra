module Utilities.ParseByteString (parseByteString) where

import Control.Applicative ((<$>))
import Data.Word
import Data.Binary.Get

data Action m = A1 (m Word8) | A2 (m Word16) | A4 (m Word32)

action 1 = A1 getWord8
action 2 = A2 getWord16be
action 4 = A4 getWord32be
action _ = A1 getWord8


getActionList xs = map action xs

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

parseByteString list byteString = cleanup solution
		where 
			cleanup list1 = map snd . filter ((/= 0) . fst) $ zip list list1
			solution = map toVal $ runGet actionList byteString
			actionList = getActions (getActionList list)
