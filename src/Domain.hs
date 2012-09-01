{-# LANGUAGE DeriveDataTypeable #-}
module Domain where

import Data.Data
import Data.Typeable

import Position

data Paddle = Paddle { y :: Float, playerName :: String } deriving (Data, Typeable, Show)
data Ball = Ball { pos :: Position } deriving (Data, Typeable, Show)
data Conf = Conf { maxWidth :: Int, maxHeight :: Int, paddleHeight :: Int, paddleWidth :: Int, ballRadius :: Int, tickInterval :: Int} deriving (Data, Typeable, Show)
data GameStatus = GameStatus { time :: Int, left :: Paddle, right :: Paddle, ball :: Ball, conf :: Conf } deriving (Data, Typeable, Show)
