-- Yksikkötestit
-- En tiedä parasta testiframeworkkia, 
-- joten ihan perus luokka...

-- GHCi: 
-- :cd D:\Muut\gitrepo\pongbot\src\
-- :load UnitTests

import Domain
import Position
import GameLogic

testStatus1 = GameStatus 1 
               (Paddle 200 "Agassi") 
               (Paddle 340 "Boris")
               (Ball (Position 100 100)) 
               (Conf 640 480 50 10 5 30)

testStatus2 = GameStatus 2 
               (Paddle 200 "Agassi") 
               (Paddle 350 "Boris")
               (Ball (Position 120 90))
               (Conf 640 480 50 10 5 30)

positionGoalTest1 = positionNearTheWall testStatus1
positionGoalTest2 = positionToLoiter testStatus1
positionGoalTest3 = positionToDefence testStatus1 testStatus2
modeTest = selectMode testStatus1 testStatus2

testResults :: IO()
testResults = do 
   putStrLn $ "positionGoalTest1 ok: " ++ (show (positionGoalTest1 == 102.5))
   putStrLn $ "positionGoalTest2 ok: " ++ (show (positionGoalTest2 == 170))
   putStrLn $ "positionGoalTest3 ok: " ++ (show (positionGoalTest3 == 147.5))
   putStrLn $ "modeTest ok: " ++ (show (case modeTest of Loiter -> True))
