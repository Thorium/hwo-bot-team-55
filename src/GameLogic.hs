-- Pelilogiikka. Best practicet:
--    - Ei mutable tilaa (vain pakollinen IO).
--    - Ei if-lauseita: http://www.antiifcampaign.com/
--    - Ei luuppeja: http://weblogs.asp.net/podwysocki/archive/2009/06/26/the-anti-for-campaign.aspx

module GameLogic where

import System.IO(Handle)
import Json
import Position
import Domain
{-
    Nelja modea:
    
    Loiter  - Pallo menee pois pain, joten nou hata... 
	          + Mennaan pallon ja laudan puoleen valiin.
    Defence - Pallo tulee kaukaa kohti
	          + Lasketaan paikka, johon pallo on tulossa.
    NearTheWall  - Pallo lahella ja kulmassa, mailat kimpoilee seinista. 
              + Yritetaan vaan saada pallo kiinni
    WaitForMoreInfo - Kaikki toistaiseksi okei
              + Odotetaan lisaa viesteja palvelimelta

    Alun perin oli viela viides mode, hyokkays, mutta luovuin siita, 
    koska mailan kontrolli palloon ei ole riittavan tarkka.
-}
data PaddleMode = Loiter | Defence | NearTheWall | WaitForMoreInfo

-- Tavoite-Y-positio modelle NearTheWall
-- Oma kulma: Mailan y = pallon y.
positionNearTheWall :: GameStatus -> Float
positionNearTheWall status = 
    let ballSize = fromIntegral $ ballRadius $ conf $ status
        ballCenter = ballSize / 2
    in
    (Position.y $ pos $ ball $ status :: Float) + ballCenter

-- Tavoite-Y-positio modelle Loiter
-- Tarkempi speksi: specs/Loiter.png
positionToLoiter :: GameStatus -> Float
positionToLoiter status = 
    let currentBallPosition = pos $ ball $ status
        boardHeight = fromIntegral $ maxHeight $ conf $ status
        paddleCenter = (fromIntegral $ paddleHeight $ conf $ status) /2
        halfBoard = boardHeight /2
    in 
    (halfBoard + (Position.y $ currentBallPosition)) /2

-- Tavoite-Y-positio modelle Defence
-- Tarkempi speksi: specs/Defence.png
positionToDefence :: GameStatus -> GameStatus -> Float
positionToDefence status previousStatus = 
    let ballMotionSlope = case (cx - px) of 
                          0 -> 0
                          v -> - (cy - py) / v -- suoran yhtalo: kulmakerroin
        positionToHitPaddleWithNoWalls = ballMotionSlope * cx + cy 
        timesToHitWallPositive = truncate (positionToHitPaddleWithNoWalls / boardHeight)
        timesToHitWall = case positionToHitPaddleWithNoWalls < 0 of
                              True -> timesToHitWallPositive - 1 -- y nolla, x-akseli, on myos seina...
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
    
-- Valitaan mika mode on otollisin kyseiselle laudan positiolle
selectMode :: GameStatus -> GameStatus -> PaddleMode
selectMode previousStatus status = 
    let comingMyWay = (px >= cx)
        distanceTravelled = sqrt((cx - px) ^ 2 + (cy - py) ^2) -- (lähde: Pythagoras, n. 500 eaa)
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

-- Mailan liikutus
movePaddle :: Handle -> Float -> IO Float
movePaddle handle speed = do
    send handle "changeDir" speed
    return speed

moveDirection :: GameStatus -> GameStatus -> Handle -> Float -> IO Float
moveDirection previousStatus status handle currentSpeed = do
    case paddleDirection of
        Nothing -> do
            putStrLn "...wait..."
            return currentSpeed
        Just(pdir)
            | pdir > tolerance -> 
                case (currentSpeed, nearTheEdge, inStartPosition) of
                     (-1.0, False, False) -> return currentSpeed 
                     (_, True, False) -> movePaddle handle (-0.75) 
                     otherwise -> movePaddle handle (-1.0) -- ylos
            | pdir < -tolerance ->
                case (currentSpeed, nearTheEdge, inStartPosition) of
                     (1.0, False, False) -> return currentSpeed
                     (_, True, False) -> movePaddle handle (0.75)
                     otherwise -> movePaddle handle (1.0) -- alas
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