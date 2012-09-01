{-# LANGUAGE DeriveDataTypeable #-}
module Position where

import Data.Data
import Data.Typeable

data Position = Position { x :: Float, y :: Float } deriving (Data, Typeable, Show)
