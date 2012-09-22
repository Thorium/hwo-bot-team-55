-- GHCi: 
-- :cd D:\Muut\gitrepo\pongbot\src\
-- :load GameLogic

module GameLogic where

import System.IO(Handle)
import Json
import Position
import Domain
{-
  Nelj� modea:
    Loiter  - Pallo menee pois p�in, joten nou h�t�... 
	          + Menn��n pallon ja laudan puoleen v�liin.
    Defence - Pallo tulee kaukaa kohti
	          + Lasketaan paikka, johon pallo on tulossa.
    NearTheWall  - Pallo l�hell� ja kulmassa, mailat kimpoilee seinist�. 
              + Yritet��n vaan saada pallo kiinni
    WaitForMoreInfo - Kaikki okei, odotetaan lis�� viestej� palvelimelta
-}
data PaddleMode = Loiter | Defence | NearTheWall | WaitForMoreInfo

positionNearTheWall :: GameStatus -> Float
positionNearTheWall status = 
      let ballSize = fromIntegral $ ballRadius $ conf $ status
          ballCenter = ballSize / 2
      in
      (Position.y $ pos $ ball $ status :: Float) + ballCenter

positionToLoiter :: GameStatus -> Float
positionToLoiter status = 
  let currentBallPosition = pos $ ball $ status
      boardHeight = fromIntegral $ maxHeight $ conf $ status
      paddleCenter = (fromIntegral $ paddleHeight $ conf $ status) /2
      halfBoard = boardHeight /2
  in 
  (halfBoard + (Position.y $ currentBallPosition)) /2

-- Paikka johon pallo tulee menem��n...
positionToDefence :: GameStatus -> GameStatus -> Float
positionToDefence status previousStatus = 
  -- pallon liike
  let ballMotionSlope = - (cy - py) / (cx - px)
      positionToHitPaddleWithNoWalls = ballMotionSlope * cx + cy
      timesToHitWallPositive = truncate (positionToHitPaddleWithNoWalls / boardHeight)
      timesToHitWall = case positionToHitPaddleWithNoWalls < 0 of
                                True -> timesToHitWallPositive - 1 -- There is wall in y = 0 also...
                                False -> timesToHitWallPositive
      positionToHitPaddle = positionToHitPaddleWithNoWalls - fromIntegral(timesToHitWall)*boardHeight
  in
  case even timesToHitWall of
    True -> positionToHitPaddle
    False -> boardHeight - positionToHitPaddle
  where
    ballSize = fromIntegral $ ballRadius $ conf $ status
    ballCenter = ballSize / 2
    boardHeight = fromIntegral $ maxHeight $ conf $ status :: Float
    coordinateZero = fromIntegral $ paddleWidth $ conf $ status :: Float
    cx = (Position.x $ pos $ ball $ status :: Float) - coordinateZero
    px = (Position.x $ pos $ ball $ previousStatus :: Float) - coordinateZero
    cy = (Position.y $ pos $ ball $ status :: Float) + ballCenter
    py = (Position.y $ pos $ ball $ previousStatus :: Float) + ballCenter
    
{-    
paddleGoalAttack :: GameStatus -> Float
paddleGoalAttack = 
  --     kulma jolla pallo saadaan nurkkaan jossa vastapelaaja ei ole
  --     tai kulma jolla pallo saadaan n sein�n kautta nurkkaan jossa vastapelaaja ei ole
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
       (True, _, _) | cx < myCorner -> NearTheWall
                    | otherwise -> WaitForMoreInfo
    where 
      ballCenter = (/) ((fromIntegral $ ballRadius $ conf $ status)::Float) 2
      cx = Position.x $ pos $ ball $ status
      px = Position.x $ pos $ ball $ previousStatus
      cy = (Position.y $ pos $ ball $ status) + ballCenter
      py = (Position.y $ pos $ ball $ previousStatus) + ballCenter
      boardHeight :: Float
      boardHeight = fromIntegral $ maxHeight $ conf $ status
      myCorner = (fromIntegral $ maxWidth $ conf $ status) / 4
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
            case (currentSpeed, nearTheEdge, inStartPosition) of
               (-1.0, False, False) -> return currentSpeed 
               (_, True, False) -> movePaddle handle (-0.75) 
               otherwise -> movePaddle handle (-1.0) -- move up
        | pdir < -tolerance ->
            case (currentSpeed, nearTheEdge, inStartPosition) of
               (1.0, False, False) -> return currentSpeed
               (_, True, False) -> movePaddle handle (0.75)
               --putStrLn $ "move down "++ (show pdir) ++ " a "  ++ (show $ Domain.y $ left $ status) ++ " mode "
               otherwise -> movePaddle handle (1.0) -- move down
        | otherwise -> 
            case (currentSpeed, nearTheEdge, inStartPosition) of
               (0.0, False, False) -> return currentSpeed
               --putStrLn $ "stop " ++ (show $ Domain.y $ left $ status) ++ " mode "
               otherwise -> movePaddle handle (0.0) -- stop
    where
        boardSize = fromIntegral $ maxHeight $ conf $ status
        paddleCenter = fromIntegral (paddleWidth $ conf $ status) /2
        ballSize = fromIntegral $ ballRadius $ conf $ status
        tolerance = case ballSize < 6 of True -> ballSize
                                         False -> 6.0
        paddleMode = selectMode previousStatus status
        paddlePosition = Domain.y $ left $ status
        paddleSize = fromIntegral $ paddleHeight $ conf $ status
        nearTheEdge = paddlePosition < paddleSize * 1.5 || paddlePosition > boardSize - paddleSize * 2.5
        inStartPosition = boardSize / 2 == paddlePosition
        paddleDirection = case paddleMode of
            Loiter -> 
                Just $ paddlePosition + paddleCenter - (positionToLoiter status)
            --Attack -> Just(fromIntegral $ positionToDefence status previousStatus)
            Defence -> 
                Just $ paddlePosition + paddleCenter - (positionToDefence status previousStatus)
            NearTheWall ->
                Just $ paddlePosition + paddleCenter - (positionNearTheWall status)                
            WaitForMoreInfo -> 
                --putStrLn $ "\n Waiting more info... \n"
                Nothing
