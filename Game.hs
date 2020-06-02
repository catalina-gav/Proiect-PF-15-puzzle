module Game where
import Data.List

type Tile = Int
data State = Running | GameOver | Instructions deriving (Eq, Show)
type List = [Tile]
type Puzzle = [[Tile]]
type Poz = (Int,Int)
data Game = Game{puzzle ::Puzzle
				,moves ::Int
				,gameState ::State
				,randomList ::List
                }deriving (Eq, Show)

n :: Int
n = 4
--pentru demo
almostSolvedPuzzle = [[0,13,14,15], [9,10,11,12], [5,6,7,8], [1,2,3,4]]
--latimea la care ne raportam
width :: Int
width = 660
--inaltimea la care ne raportam
height :: Int
height = 480
--latimea unei casute
tileWidth :: Float
tileWidth = fromIntegral width / fromIntegral n
--inaltimea unei casute
tileHeight :: Float
tileHeight = fromIntegral height / fromIntegral n
--tranforma o lista intr-o matrice de 4 x 4
splitList :: [Int] -> [[Int]]
splitList [] = []
splitList (l1 : l2 : l3 : l4 : tl) = ( [l1] ++ [l2] ++ [l3] ++ [l4] ) : (splitList tl)
splitList l = [l]
--jocul initial
initialGame gen state= Game { puzzle = splitList(gen)
					   , moves = 0
					   , randomList = gen
					   , gameState = state
                   }
--demo rezolvare
-- initialGame gen state= Game { puzzle = almostSolvedPuzzle
					   -- , moves = 0
					   -- , randomList = gen
					   -- , gameState = state
                   -- }