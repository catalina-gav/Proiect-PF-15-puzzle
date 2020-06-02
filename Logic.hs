module Logic where
import Game
import Graphics.Gloss.Interface.Pure.Game
import Functii
import System.Random
import Data.Array.IO
import Control.Monad
import System.IO.Unsafe

isCoordCorrect = inRange ((0, 0), (n - 1, n - 1))
solution = [[13,14,15,0], [9,10,11,12], [5,6,7,8], [1,2,3,4]]
checkGameOver :: Game -> Bool
checkGameOver game
    | boards == solution = True
    | otherwise = False
    where boards = puzzle game
mouseHandle :: Game -> (Int, Int) -> Game
mouseHandle game cellCoord
	| checkGameOver game == True = game { gameState = GameOver}
    | isCoordCorrect cellCoord &&  (elem (fst(cellCoord),snd(cellCoord)) (availableZero matrix) == True) =
         game {  puzzle = newPuzzle
				,moves = score +1
			  }
    | otherwise = game
    where 
		 score = moves game
		 matrix = puzzle game
		 newPuzzle =  move (getValoare matrix (fst(cellCoord),snd(cellCoord))) (puzzle game) 

keyHandle :: Game -> String -> Game
keyHandle game key
	| key == "enter" = game { gameState = Running}
	| key == "i" = game { gameState = Instructions}
	| checkGameOver game == True = game { gameState = GameOver}
    | isCoordCorrect (firstZero matrix,(secondZero matrix)+1) && key=="left" =
        game { puzzle = leftMatrix
				,moves = score +1
			}
	| isCoordCorrect (firstZero matrix,(secondZero matrix)-1) && key=="right" =
         game { puzzle = rightMatrix
				,moves = score +1
				}
	| isCoordCorrect ((firstZero matrix) -1,secondZero matrix) && key=="up" =
        game { puzzle = upMatrix
			    ,moves = score +1
				}
	| isCoordCorrect ((firstZero matrix) +1,secondZero matrix) && key=="down" =
         game { puzzle = downMatrix
				,moves = score +1
				}
    | otherwise = game	
    where 
		score = moves game
		matrix = puzzle game
		downMatrix = move (getValoare matrix ((firstZero matrix) +1,secondZero matrix)) matrix
		upMatrix = move (getValoare matrix ((firstZero matrix) -1,secondZero matrix)) matrix
		leftMatrix = move ( getValoare matrix  (firstZero matrix,(secondZero matrix)+1)) matrix
		rightMatrix = move (getValoare matrix (firstZero matrix,(secondZero matrix)-1)) matrix
		
mousePosAsCellCoord :: (Float, Float) -> (Int, Int)
mousePosAsCellCoord (x, y) = ( floor ((y + (fromIntegral screenHeight * 0.5)) / cellHeight)
                             , floor ((x + (fromIntegral screenWidth * 0.5)) / cellWidth)
                             )
updateGame :: Event -> Game -> Game
updateGame (EventKey (MouseButton LeftButton) _ _ mousePos) game =
    case gameState game of
		Running -> mouseHandle game $ mousePosAsCellCoord mousePos
		GameOver-> game
		Instructions -> game
		
updateGame (EventKey (Char char) Down _ _) game 
        | char == 'w' = 
		case gameState game of
		Running -> keyHandle game "up"
		GameOver-> game
		Instructions -> game
        | char == 'a' = 
		case gameState game of
		Running -> keyHandle game "left"
		GameOver-> game
		Instructions -> game
        | char == 's' = 
		case gameState game of
		Running -> keyHandle game "down"
		GameOver-> game
		Instructions -> game
        | char == 'd' =
		case gameState game of
		Running -> keyHandle game "right"
		GameOver-> game
		Instructions -> game
		| char == 'r' = 
		case gameState game of
		Running -> initialGame (randomList game) Running
		GameOver -> initialGame (randomList game) Running
		Instructions -> game
		| char == 'n' = 
		case gameState game of
		Running -> initialGame (unsafePerformIO $ shuffle [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15]) Running
		GameOver -> initialGame (unsafePerformIO $ shuffle [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15]) Running
		Instructions -> game
		| char == 'i' = 
		case gameState game of
		Running -> keyHandle game "i"
		GameOver -> keyHandle game "i"
		Instructions -> game

updateGame (EventKey (SpecialKey KeyLeft) Down _ _) game =
    case gameState game of
		Running -> keyHandle game "left"
		GameOver-> game
		Instructions -> game
updateGame (EventKey (SpecialKey KeyRight) Down _ _) game =
    case gameState game of
		Running -> keyHandle game "right"
		GameOver-> game
		Instructions -> game
updateGame (EventKey (SpecialKey KeyUp) Down _ _) game =
    case gameState game of
		Running -> keyHandle game "up"
		GameOver-> game
		Instructions -> game
updateGame (EventKey (SpecialKey KeyEnter) Down _ _) game =
    case gameState game of
		Running -> game
		GameOver -> game
		Instructions -> keyHandle game "enter"
updateGame (EventKey (SpecialKey KeyDown) Down _ _) game =
    case gameState game of
		Running -> keyHandle game "down"
		GameOver-> game
		Instructions -> game
updateGame _ game = game

shuffle :: [a] -> IO [a]
shuffle xs = do
        ar <- newArray n xs
        forM [1..n] $ \i -> do
            j <- randomRIO (i,n)
            vi <- readArray ar i
            vj <- readArray ar j
            writeArray ar j vi
            return vj
  where
    n = length xs
    newArray :: Int -> [a] -> IO (IOArray Int a)
    newArray n xs =  newListArray (1,n) xs