module GameLogic where

import System.IO(Handle)

import Json

moveUp :: Handle -> IO ()
moveUp handle =
  send handle "changeDir" direction
  where direction = (-1.0) :: Float

