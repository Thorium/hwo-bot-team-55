-- GHCi: 
-- :cd D:\Muut\gitrepo\pongbot\src\
-- :load GameLogic

module GameLogic where

import System.IO(Handle)
import Json
import Position
import Domain
{-
  Kolme modea:
    Loiter  - Pallo menee pois päin, joten nou hätä... 
	          + Mennään pallon ja laudan puoleen väliin.
    Defence - Pallo tulee kaukaa kohti
	          + Lasketaan paikka, johon pallo on tulossa.
    Attack  - Pallo lähellä, valitaan mihin kohtaan lyödään. 
	          + Yritetään 0-n pompulla siihen kulmaan, jossa vastustajan maila ei ole.
-}
data PaddleMode = Loiter | Defence | Attack

positionToLoiter :: GameStatus -> Float
positionToLoiter status = 
  let currentBallPosition = pos $ ball $ status
      boardHeight = fromIntegral $ maxHeight $ conf $ status
      paddleCenter = (fromIntegral $ paddleHeight $ conf $ status) /2
      halfBoard = boardHeight /2
  in 
  (halfBoard + (Position.y $ currentBallPosition)) /2

-- Paikka johon pallo tulee menemään...
-- TODO: suoraan tarvitaan itse asiassa 3 pistettä, ettei pallo ole kimmonnut seinästä välillä...
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
      
{-    
paddleGoalAttack :: GameStatus -> Float
paddleGoalAttack = 
  --     kulma jolla pallo saadaan nurkkaan jossa vastapelaaja ei ole
  --     tai kulma jolla pallo saadaan n seinän kautta nurkkaan jossa vastapelaaja ei ole
  --     - Suora on nopein x-suunnassa ilman pomppuja, mutta sivuttaisnopeus pallolle 
  --       mailan nopeutta suuremmaksi saadaan pompuilla
-}
  
selectMode :: GameStatus -> GameStatus -> PaddleMode
selectMode status previousStatus =
    let currentBallXPosition = Position.x $ pos $ ball $ status
        previousBallXPosition = Position.x $ pos $ ball $ previousStatus
        comingMyWay = (previousBallXPosition >= currentBallXPosition)
        boardWidth = maxWidth $ conf $ status
        optimiHyokkaysPiste = 0.0 --TODO: Optimoi
        optimiPuolustusPiste = fromIntegral boardWidth --TODO: Optimoi
    in
    case (currentBallXPosition, comingMyWay) of
      (_, False) -> Loiter
      (x, _) 
             | x < optimiHyokkaysPiste -> Attack
             | x > optimiPuolustusPiste -> Loiter
             | otherwise -> Defence


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
    | paddleDirection == 0 = moveStop handle
    | paddleDirection > 0 = moveUp handle
    | paddleDirection < 0 = moveDown handle
    where
       previousStatus = status -- TODO: State monad to give the previous state
       goal = case (time previousStatus) == (time status) of
            True -> Position.y $ pos $ ball $ status
            False -> case selectMode status previousStatus of
                        Loiter -> positionToLoiter status
                        Attack -> (fromIntegral $ positionToDefence status previousStatus)
                        Defence -> (fromIntegral $ positionToDefence status previousStatus)
       paddleCenter = fromIntegral (paddleWidth $ conf $ status) /2
       paddleDirection = (Domain.y $ left $ status) - paddleCenter - (fromIntegral $ positionToDefence status previousStatus)
