module Main where

import Graphics.Gloss
import Graphics.Gloss.Data.Color

import Game
import Logic
import Rendering

window = InWindow "15-puzzle" (screenWidth+450, screenHeight+200) (20, 50)
backgroundColor = mixColors  95 5 black cyan

main :: IO ()
main = do
   gen <- shuffle [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15]
   startGame gen
   return()

startGame gen = play window backgroundColor 30 (initialGame gen Instructions) gameAsPicture updateGame (const id)