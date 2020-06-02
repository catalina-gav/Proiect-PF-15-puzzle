module Game where

import Data.List


list= [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15]

type Cell = Int
data State = Running | GameOver | Instructions deriving (Eq, Show)
type Cells = [Cell]
type Puzzle = [[Cell]]
type Poz = (Int,Int)
data Game = Game {	puzzle :: Puzzle
					, moves :: Int
					, gameState :: State
					, randomList :: Cells
                 } deriving (Eq, Show)

n :: Int
n = 4

almostSolvedPuzzle = [[13,14,0,15], [9,10,11,12], [5,6,7,8], [1,2,3,4]]

screenWidth :: Int
screenWidth = 660

screenHeight :: Int
screenHeight = 480

cellWidth :: Float
cellWidth = fromIntegral screenWidth / fromIntegral n

cellHeight :: Float
cellHeight = fromIntegral screenHeight / fromIntegral n

splitList :: [Int] -> [[Int]]
splitList [] = []
splitList (l1 : l2 : l3 : l4 : tl) = ( [l1] ++ [l2] ++ [l3] ++ [l4] ) : (splitList tl)
splitList l = [l]

initialGame gen state= Game { puzzle = almostSolvedPuzzle
					   , moves = 0
					   , randomList = gen
					   , gameState = state
                   }
