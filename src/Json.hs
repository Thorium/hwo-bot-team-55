{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}
module Json where

import System.IO(Handle, hFlush, hPutChar)
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.Aeson.Generic as GJ

send :: ToJSON a => Handle -> String -> a -> IO ()
send handle msgType msgData = do
  let json = encode $ object ["msgType" .= msgType, "data" .= msgData]
  L.hPut handle $ json
  hPutChar handle '\n'
  hFlush handle

decodeMessage message = decode message >>= (fromOk . fromJSON)

parseData messageData = fromOk $ GJ.fromJSON messageData

fromOk (Success x) = x

instance FromJSON (String, Value) where
  parseJSON (Object v) = do
    msgType <- v .: "msgType"
    msgData <- v .: "data"
    return (msgType, msgData)
  parseJSON x          = fail $ "Not an JSON object: " ++ (show x)
