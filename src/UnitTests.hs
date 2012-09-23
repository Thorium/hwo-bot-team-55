{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}
module UnitTests where

-- Yksikkötestit
-- En tiedä parasta testiframeworkkia, 
-- joten ihan perus luokka...

-- GHCi: 
-- :cd D:\Muut\gitrepo\pongbot\src\
-- :load UnitTests

import Domain
import Position
import GameLogic

player1 = (Paddle 200 "Agassi") 
player2 = (Paddle 340 "Boris")
testconf = (Conf 900 300 20 0 0 0)
createPosition x y n = GameStatus n player1 player2 (Ball (Position x y)) testconf

p01 = createPosition 300 100  1
p02 = createPosition 600 100  2
p03 = createPosition 100 200  1
p04 = createPosition 400 200  1
p05 = createPosition 500 200  1
p06 = createPosition 700 200  1

defenceTest = [(100 == positionToDefence p06 p02),
               (200 == positionToDefence p04 p01),
               (100 == positionToDefence p02 p01),
               (250 == positionToDefence p01 p03),
               (200 == positionToDefence p02 p04),
               (100 == positionToDefence p02 p05),
               (25 == positionToDefence p06 p01)]

p07 = createPosition 30 10    2
p08 = createPosition 80 10    1
p09 = createPosition 30 290   2
p10 = createPosition 80 290   1
p11 = createPosition 600 290  2
p12 = createPosition 650 290  1
p13 = createPosition 310 100  0

nearTheWallTest = [(10 == positionNearTheWall p07), 
                   (290 == positionNearTheWall p09)]

loiterTest = [(125 == positionToLoiter p01),
              (175 == positionToLoiter p03),
              (80 == positionToLoiter p07),
              (220 == positionToLoiter p09)]

instance Show PaddleMode where
    show Loiter = "Loiter"
    show Defence = "Defence"
    show NearTheWall = "NearTheWall"
    show WaitForMoreInfo = "WaitForMoreInfo"
   
modeCheck a1 r1 = (show r1) == (show a1)
                           
modeTest = [modeCheck Loiter          (selectMode p01 p02),
            modeCheck Defence         (selectMode p13 p01),
            modeCheck NearTheWall     (selectMode p08 p07),
            modeCheck NearTheWall     (selectMode p10 p09),
            modeCheck WaitForMoreInfo (selectMode p12 p11)]

-- GHCi:
-- defenceTest      [True,True,True,True,True,True,True]
-- modeTest         [True,True,True,True,True]
-- nearTheWallTest  [True,True]
-- loiterTest       [True,True,True,True]