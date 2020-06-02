module Main where

import Graphics.Gloss
import Graphics.Gloss.Data.Color
import Game
import Logic
import Rendering
import System.Random.Shuffle

list= [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15]
solutionList = [13,14,15,0, 9,10,11,12, 5,6,7,8, 1,2,3,4]

screenWidth :: Int
screenWidth = 1110

screenHeight :: Int
screenHeight = 680

window = InWindow "15-puzzle" (screenWidth, screenHeight) (20, 50)
backgroundColor = mixColors  95 5 black cyan
--generez lista initiala
main :: IO ()
main = do
   gen <- shuffleM list
   putStrLn (show(gen))
   if gen == solutionList
	then main
	else startGame gen

startGame gen = play window backgroundColor 30 (initialGame gen Instructions) gameAsPicture updateGame (const id)