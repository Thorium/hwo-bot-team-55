-- GHCi: 
-- :cd D:\Muut\gitrepo\pongbot\src\
-- :load GameLogic

module GameLogic where

import System.IO(Handle)
import Json
import Position
import Domain

data PaddleMode = Loiter | Defence | Attack

findPosition :: GameStatus -> GameStatus -> Float
findPosition status previousStatus = 
  let previousBallPosition = pos $ ball $ previousStatus
      currentBallPosition = pos $ ball $ status

      board = (fromIntegral (maxWidth $ conf $ status), fromIntegral (maxHeight $ conf $ status))

      paddleCenter = fromIntegral (paddleWidth $ conf $ status) /2
      halfBoard = fst board /2
      paddleGoalLoiter = (halfBoard + (Position.y $ currentBallPosition)) /2
  
  -- let paddleGoalAttack = 
  --     kulma jolla pallo saadaan nurkkaan jossa vastapelaaja ei ole
  --     tai kulma jolla pallo saadaan seinän kautta nurkkaan jossa vastapelaaja ei ole

      comingMyWay = (Position.y $ previousBallPosition) >= (Position.y $ currentBallPosition)
  in paddleGoalLoiter
--  let chooseMode =
--    (Position.x $ currentBallPosition), comingMyWay
--    | _, false -> PaddleMode.loiter
--    | x, _ when x <= optimiHyökkäysPiste -> PaddleMode.attack
--    | x, _ when x <= optimiPuolustusPiste -> PaddleMode.defence
--    | x, _ when x > optimiPuolustusPiste -> PaddleMode.loiter


  -- -------------
  --
  --
  --               |-- halfBoard
  --             C --- Paddle.y
  --    .          --- Ball.y
  -- -------------


-- Paikka johon pallo tulee menemään...
positionToDefence :: GameStatus -> GameStatus -> Int
positionToDefence status previousStatus = 
  -- pallon liike
  let intervalsToHitPaddle = (cx - px) / cx
      positionToHitPaddleWithNoWalls = round $ cy + intervalsToHitPaddle * (cy - py)
      timesToHitWall = positionToHitPaddleWithNoWalls `quot` boardHeight
      positionToHitPaddle = positionToHitPaddleWithNoWalls `rem` boardHeight
  in
  case even timesToHitWall of
    True -> positionToHitPaddle
    False -> boardHeight - positionToHitPaddle
  where
    cx = Position.x $ pos $ ball $ status
    px = Position.x $ pos $ ball $ previousStatus
    cy = Position.y $ pos $ ball $ status
    py = Position.y $ pos $ ball $ previousStatus
    boardHeight = maxHeight $ conf $ status
      

	--  |         *
	--  |        /    +
	--  |       /
	--  -------/-----
	--  |     /
	--  |    /        -
	--  |   /
	--  ---/---------
	--  | /
	--  |/            +
	--  x
	--  -------------
	--  |
	--  |             -
	--  |
	--  -------------


--speedIntervalCalculation = 
--   nykyinen intervalli miinus aiempi intervalli
--   vauhti jolla haluttu paikka saavutetaan
--   (paikka - nykypaikka) / intervallimuutos 

moveUp :: Handle -> IO ()
moveUp handle =
  send handle "changeDir" direction
  where direction = (-1.0) :: Float

moveDown :: Handle -> IO ()
moveDown handle =
  send handle "changeDir" direction
  where direction = (1.0) :: Float

moveStop :: Handle -> IO ()
moveStop handle =
  send handle "changeDir" direction
  where direction = (0.0) :: Float

	
moveDirection :: GameStatus -> Handle -> IO()
moveDirection status handle
    | paddleMinusGoal == 0 = moveStop handle
    | paddleMinusGoal > 0 = moveUp handle
    | paddleMinusGoal < 0 = moveDown handle
    where
       previousStatus = status -- TODO: State monad to give the previous state
       paddleMinusGoal = (Domain.y $ left $ status) - (fromIntegral $ positionToDefence status previousStatus)

