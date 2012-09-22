-- GHCi: 
-- :cd D:\Muut\gitrepo\pongbot\src\
-- :load GameLogic

module GameLogic where

import System.IO(Handle)
import Json
import Position
import Domain
{-
  Neljä modea:
    Loiter  - Pallo menee pois päin, joten nou hätä... 
	          + Mennään pallon ja laudan puoleen väliin.
    Defence - Pallo tulee kaukaa kohti
	          + Lasketaan paikka, johon pallo on tulossa.
    Attack  - Pallo lähellä, valitaan mihin kohtaan lyödään. 
	          + Yritetään 0-n pompulla siihen kulmaan, jossa vastustajan maila ei ole.
    WaitForMoreInfo - Kaikki okei, odotetaan lisää viestejä palvelimelta
-}
data PaddleMode = Loiter | Defence | Attack | WaitForMoreInfo

positionToLoiter :: GameStatus -> Float
positionToLoiter status = 
  let currentBallPosition = pos $ ball $ status
      boardHeight = fromIntegral $ maxHeight $ conf $ status
      paddleCenter = (fromIntegral $ paddleHeight $ conf $ status) /2
      halfBoard = boardHeight /2
  in 
  (halfBoard + (Position.y $ currentBallPosition)) /2

-- Paikka johon pallo tulee menemään...
positionToDefence :: GameStatus -> GameStatus -> Float
positionToDefence status previousStatus = 
  -- pallon liike
  let ballMotionSlope = - (cy - py) / (cx - px)
      positionToHitPaddleWithNoWalls = ballMotionSlope * cx + cy
      timesToHitWall = truncate (positionToHitPaddleWithNoWalls / boardHeight)
      positionToHitPaddle = positionToHitPaddleWithNoWalls - fromIntegral(timesToHitWall)*boardHeight
  in
  case even timesToHitWall of
    True -> positionToHitPaddle
    False -> boardHeight - positionToHitPaddle
  where
    boardHeight = fromIntegral $ maxHeight $ conf $ status :: Float
    coordinateZero = fromIntegral $ paddleWidth $ conf $ status :: Float
    cx = (Position.x $ pos $ ball $ status :: Float) - coordinateZero
    px = (Position.x $ pos $ ball $ previousStatus :: Float) - coordinateZero
    cy = Position.y $ pos $ ball $ status :: Float
    py = Position.y $ pos $ ball $ previousStatus :: Float
    
{-    
paddleGoalAttack :: GameStatus -> Float
paddleGoalAttack = 
  --     kulma jolla pallo saadaan nurkkaan jossa vastapelaaja ei ole
  --     tai kulma jolla pallo saadaan n seinän kautta nurkkaan jossa vastapelaaja ei ole
  --     - Suora on nopein x-suunnassa ilman pomppuja, mutta sivuttaisnopeus pallolle 
  --       mailan nopeutta suuremmaksi saadaan pompuilla
-}
  
selectMode :: GameStatus -> GameStatus -> PaddleMode
selectMode previousStatus status = 
    let comingMyWay = (px >= cx)
        distanceTravelled = sqrt((cx - px) ^ 2 + (cy - py) ^2)
        fromBottom = boardHeight - cy
        nearTheWall = (cy < distanceTravelled) || (fromBottom < distanceTravelled)
        boardWidth = maxWidth $ conf $ status
    in
    case (nearTheWall, comingMyWay, timeStampOk) of
       (_, _, False) -> WaitForMoreInfo
       (_, False, _) -> Loiter
       (False, _, _) -> Defence
       (True, _, _) -> WaitForMoreInfo
    where 
      cx = Position.x $ pos $ ball $ status
      px = Position.x $ pos $ ball $ previousStatus
      cy = Position.y $ pos $ ball $ status
      py = Position.y $ pos $ ball $ previousStatus
      boardHeight :: Float
      boardHeight = fromIntegral $ maxHeight $ conf $ status
      timeStampOk = (time $ status) > (time $ previousStatus)

--speedIntervalCalculation = 
--   nykyinen intervalli miinus aiempi intervalli
--   vauhti jolla haluttu paikka saavutetaan
--   (paikka - nykypaikka) / intervallimuutos 

movePaddle :: Handle -> Float -> IO Float
movePaddle handle speed = do
    send handle "changeDir" speed
    return speed

moveDirection :: GameStatus -> GameStatus -> Handle -> Float -> IO Float
moveDirection previousStatus status handle currentSpeed = do
    case paddleDirection of
      Nothing -> 
         do putStrLn "...wait..."
            return currentSpeed
      Just(pdir)
        | pdir > tolerance -> 
            --putStrLn $ "move up " ++ (show pdir) ++ " a " ++ (show $ Domain.y $ left $ status) ++ " mode "
            case currentSpeed of
               (-1.0) -> return currentSpeed 
               otherwise -> movePaddle handle (-1.0) -- move up
        | pdir < -tolerance ->
            case currentSpeed of
               (1.0) -> return currentSpeed
               --putStrLn $ "move down "++ (show pdir) ++ " a "  ++ (show $ Domain.y $ left $ status) ++ " mode "
               otherwise -> movePaddle handle (1.0) -- move down
        | otherwise -> 
            case currentSpeed of
               (0.0) -> return currentSpeed
               --putStrLn $ "stop " ++ (show $ Domain.y $ left $ status) ++ " mode "
               otherwise -> movePaddle handle (0.0) -- stop
    where
        paddleCenter = fromIntegral (paddleWidth $ conf $ status) /2
        ballSize = fromIntegral $ ballRadius $ conf $ status
        tolerance = case ballSize < 6 of True -> ballSize
                                         False -> 6.0
        paddleMode = selectMode previousStatus status
        paddleDirection = case paddleMode of
            Loiter -> 
                Just $ (Domain.y $ left $ status) + paddleCenter - (positionToLoiter status)
            --Attack -> Just(fromIntegral $ positionToDefence status previousStatus)
            Defence -> 
                Just $ (Domain.y $ left $ status) + paddleCenter - (positionToDefence status previousStatus)
            WaitForMoreInfo -> 
                --putStrLn $ "\n Waiting more info... \n"
                Nothing
