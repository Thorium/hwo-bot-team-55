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
    NearTheWall  - Pallo lähellä ja kulmassa, mailat kimpoilee seinistä. 
              + Yritetään vaan saada pallo kiinni
    WaitForMoreInfo - Kaikki okei, odotetaan lisää viestejä palvelimelta
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

-- Paikka johon pallo tulee menemään...
positionToDefence :: GameStatus -> GameStatus -> Float
positionToDefence status previousStatus = 
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
       (True, _, _) | (cx < myCorner) && comingMyWay -> NearTheWall
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
            case (currentSpeed, nearTheEdge, inStartPosition) of
               (-1.0, False, False) -> return currentSpeed 
               (_, True, False) -> movePaddle handle (-0.75) 
               otherwise -> movePaddle handle (-1.0) -- move up
        | pdir < -tolerance ->
            case (currentSpeed, nearTheEdge, inStartPosition) of
               (1.0, False, False) -> return currentSpeed
               (_, True, False) -> movePaddle handle (0.75)
               otherwise -> movePaddle handle (1.0) -- move down
        | otherwise -> 
            case (currentSpeed, nearTheEdge, inStartPosition) of
               (0.0, False, False) -> return currentSpeed
               otherwise -> movePaddle handle (0.0) -- stop
    where
        boardSize = fromIntegral $ maxHeight $ conf $ status
        paddleCenter = fromIntegral (paddleWidth $ conf $ status) /2
        ballSize = fromIntegral $ ballRadius $ conf $ status
        tolerance = case ballSize < 4 of True -> ballSize
                                         False -> 4.0
        paddleMode = selectMode previousStatus status
        paddlePosition = Domain.y $ left $ status
        paddleSize = fromIntegral $ paddleHeight $ conf $ status
        nearTheEdge = paddlePosition < paddleSize * 1.5 || paddlePosition > boardSize - paddleSize * 2.5
        inStartPosition = boardSize / 2 == paddlePosition
        paddleDirection = case paddleMode of
            Loiter -> 
                Just $ paddlePosition + paddleCenter - (positionToLoiter status)
            Defence -> 
                Just $ paddlePosition + paddleCenter - (positionToDefence status previousStatus)
            NearTheWall ->
                Just $ paddlePosition + paddleCenter - (positionNearTheWall status)
            WaitForMoreInfo -> 
                Nothing
