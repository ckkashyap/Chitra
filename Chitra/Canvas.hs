module Chitra.Canvas where

import Control.Monad
import System.IO

import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Char
import Data.Binary.Get
import Data.Binary.Put
import Data.Word

import qualified RFB.Constants as RFB
import qualified RFB.Server as SERVER
import qualified RFB.Handshake as Handshake
import qualified RFB.CommandLoop as CommandLoop



start w h p= do
	SERVER.serve w h p doit

doit w h hnd = do
	Handshake.handshake w h hnd
	CommandLoop.commandLoop hnd w h
	

