{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}
import System.Environment(getArgs, getProgName)

import Network
import Control.Monad
import System.IO
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as L

import Json
import Domain
import Position
import GameLogic

-- Normaali peli tai myohemmin lisatty duel-mahdollisuus
data GameType = Battle | Duel

main = do
    getArgs >>= startApplication

startApplication (name:host:port:duel:_) =
    connectSocket host (read port :: Integer) >>= (startGame (name,duel) Duel)
startApplication (name:host:port:_) =
    connectSocket host (read port :: Integer) >>= (startGame name Battle)
startApplication _ =
    getProgName >>=
      (\progName -> putStrLnToStderr $ "\nUsage: " ++ (show progName) ++ " <name> <host> <port> (<duelOpponent>)")

connectSocket host port = connectTo host (PortNumber $ fromInteger port)

startGame names gameType handle = do
    case gameType of 
        Battle -> send handle "join" names
        Duel -> send handle "requestDuel" names
    putStrLnToStderr $ "Starting..."
    handleMessages handle 0.0

-- Oikeaoppinen funktionaalinen paritus ;-)
messagePairs :: [L.ByteString] -> [(L.ByteString,L.ByteString)]
messagePairs mylist = zipWith (,) mylist (tail mylist)

-- Pelilogiikalle menee kaksi viestia kerrallaan
-- Lisaksi rekursiossa nykyinen mailan liike, jotta serveria ei pommiteta turhaan
decodeMessagePair :: (L.ByteString,L.ByteString) -> Handle -> Float -> IO()
decodeMessagePair pair handle currentSpeed = 
    case decodeMessage pair of
        (Just(messageType, messageData1),Just(messageType2, messageData2)) 
            | (messageType == "gameIsOn" && messageType2 == "gameIsOn") -> do
                putStrLn $ gameStatusMessage $ parseData $ messageData1 
                newSpeed <- moveDirection (parseData $ messageData1) (parseData $ messageData2) handle currentSpeed
                handleMessages handle newSpeed
        (Just(messageType, messageData),_)
            | messageType == "gameIsOn" -> do
                putStrLn "Got first game on message..."
                putStrLn $ gameStatusMessage $ parseData $ messageData
                handleMessages handle 0.0
            | otherwise ->  do
                handleMessage handle messageType messageData
                handleMessages handle 0.0
        (Nothing,_) -> fail $ "Error parsing JSON1: " ++ (show $ fst $ pair)

-- <malli-templatesta>
-- Luuppi oli jo templatessa, menkoon taman kerran...
handleMessages handle currentSpeed = do
    lines <- liftM (L.split '\n') $ L.hGetContents handle
    forM_ (messagePairs lines) $ \msg -> do
        decodeMessagePair msg handle currentSpeed

handleMessage :: Handle -> String -> Value -> IO ()
handleMessage handle "joined" messageData = do
    putStrLnToStderr $ gameJoinedMessage $ parseData $ messageData
handleMessage handle "gameStarted" messageData = do
    putStrLn $ gameStartedMessage $ parseData $ messageData
handleMessage handle "gameIsOver" messageData = do
    putStrLn $ gameOverMessage $ parseData $ messageData
handleMessage handle messageType messageData = do
    putStrLn $ "<< Unregognized message: " ++ (show messageType) ++ " " ++ (show messageData)

gameJoinedMessage :: String -> String
gameJoinedMessage url = "\nGame visualization URL " ++ url

gameOverMessage :: String -> String
gameOverMessage winner = "<< Game over. Winner: " ++ winner

gameStartedMessage :: [String] -> String
gameStartedMessage players =
    foldl (\x y -> x ++ " " ++ y) "<< Game started with:" players

gameStatusMessage :: GameStatus -> String
gameStatusMessage status =
    "<< Game is on:"
        ++ " Player " ++ leftPlayerName ++ " at " ++ leftPlayerPosition
        ++ " Player " ++ rightPlayerName ++ " at " ++ rightPlayerPosition
        ++ " Ball at " ++ ballX ++ ", " ++ ballY
        ++ " at time " ++ timestamp
    where leftPlayerName = show $ playerName $ left $ status
          leftPlayerPosition = show $ Domain.y $ left $ status
          rightPlayerName = show $ playerName $ right $ status
          rightPlayerPosition = show $ Domain.y $ right $ status
          ballX = show $ Position.x $ pos $ ball $ status
          ballY = show $ Position.y $ pos $ ball $ status
          timestamp = show $ time $ status

putStrLnToStderr = hPutStrLn stderr
-- </malli-templatesta>
