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

main = do
  getArgs >>= startApplication

startApplication (name:host:port:_) =
  connectSocket host (read port :: Integer) >>= (startGame name)
startApplication _ =
  getProgName >>=
    (\progName -> putStrLnToStderr $ "\nUsage: " ++ (show progName) ++ " <name> <host> <port>")

connectSocket host port = connectTo host (PortNumber $ fromInteger port)

startGame name handle = do
  send handle "join" name
  handleMessages handle

handleMessages handle = do
  lines <- liftM (L.split '\n') $ L.hGetContents handle
  forM_ lines $ \msg -> do
    case decodeMessage msg of
      Just (messageType, messageData) -> do
        handleMessage handle messageType messageData
      Nothing -> fail $ "Error parsing JSON: " ++ (show msg)

handleMessage :: Handle -> String -> Value -> IO ()
handleMessage handle "joined" messageData = do
  putStrLnToStderr $ gameJoinedMessage $ parseData $ messageData
handleMessage handle "gameIsOn" messageData = do
  putStrLn $ gameStatusMessage $ parseData $ messageData
  moveDirection (parseData $ messageData) handle
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
  where leftPlayerName = show $ playerName $ left $ status
        leftPlayerPosition = show $ Domain.y $ left $ status
        rightPlayerName = show $ playerName $ right $ status
        rightPlayerPosition = show $ Domain.y $ right $ status
        ballX = show $ Position.x $ pos $ ball $ status
        ballY = show $ Position.y $ pos $ ball $ status

putStrLnToStderr = hPutStrLn stderr





