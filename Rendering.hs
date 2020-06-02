module Rendering where

import Graphics.Gloss
import Game

bgColor = mixColors  95 5 black cyan
puzzleOutline = mixColors 80 20 black bgColor
cellColor = mixColors 70 30 white bgColor
-- translatez fiecare text in celula lui din tabel, desenez dreptunghiurile din spatele numerelor
numberToCell :: Picture -> (Int,Int) -> Picture
numberToCell picture (row, column) = pictures [(translate z t (color cellColor $ rectangleSolid  (cellWidth-15)(cellHeight-15))),(translate x y picture)]
    where 
			x = fromIntegral column * cellWidth + cellWidth * 0.35
			y = fromIntegral row * cellHeight + cellHeight * 0.35
			z = fromIntegral column * cellWidth + cellWidth * 0.5
			t = fromIntegral row * cellHeight + cellHeight * 0.5
-- ac lucru dar fiind zero nu mai desenez un dreptunghi in spate			  
numberToCellZero :: Picture -> (Int,Int) -> Picture
numberToCellZero picture (row, column) = (translate x y picture)
    where 
			x = fromIntegral column * cellWidth + cellWidth * 0.06
			y = fromIntegral row * cellHeight + cellHeight * 0.06
		
--transform numerele in picturi
printNumber :: String -> Picture
printNumber "0" = scale 0.5 0.5 (text " ")
printNumber s = scale 0.5 0.5 (text s)



puzzleAsRunningPicture :: Puzzle -> Int -> Picture
puzzleAsRunningPicture puzzle score =
    pictures [ translate (cellWidth+cellWidth) (cellHeight +cellHeight) (color puzzleOutline $ rectangleSolid (fromIntegral screenWidth +20) (fromIntegral screenHeight +20))
				,color black $ cellsToPicture puzzle
			  , guiScore score
			  , restartGame
			  , newGame
			  ,gameInstructions
             ]

puzzleCells :: Cells -> Puzzle -> [Picture] 
puzzleCells  [] _ = []
puzzleCells (0:xs) puzzle = (numberToCellZero (printNumber (show 0)) (findPoz puzzle 0)) : (puzzleCells xs puzzle)
puzzleCells (x:xs) puzzle = (numberToCell (printNumber (show x)) (findPoz puzzle x)) : (puzzleCells xs puzzle)

cellsToPicture :: Puzzle -> Picture
cellsToPicture puzzle = pictures ( puzzleCells (concat puzzle) puzzle )

guiGameOver :: Int->Picture
guiGameOver score = pictures [translate (110) (fromIntegral screenHeight -100) (scale 0.3 0.3 (color cellColor (text("MOVES: " ++ (show score))))),
						translate (10) (fromIntegral screenHeight -180) (scale 0.3 0.3 (color cellColor (text "You solved the 15-Puzzle!"))),
						translate (10) (fromIntegral screenHeight -220) (scale 0.2 0.2 (color cellColor (text "Press R to restart game."))),
						translate (10) (fromIntegral screenHeight -260) (scale 0.2 0.2 (color cellColor (text "Press N to start New Game."))),
						translate (10) (fromIntegral screenHeight -300) (scale 0.2 0.2 (color cellColor (text "Press I to see the Instructions.")))]
					
guiScore :: Int->Picture 
guiScore  score = translate (110) ((fromIntegral screenHeight +20)) (scale 0.3 0.3 (pictures [message]))
      where
        message = color white (Text text)
        text = "MOVES: " ++ (show (score))
restartGame :: Picture
restartGame =translate (5) ((fromIntegral screenHeight -515)) (scale 0.2 0.2 (pictures [message]))
      where
        message = color white (Text text)
        text = "Press R to restart game."
newGame :: Picture
newGame =translate (5) ((fromIntegral screenHeight -540)) (scale 0.2 0.2 (pictures [message]))
      where
        message = color white (Text text)
        text = "Press N to start New Game."
gameInstructions :: Picture
gameInstructions = translate (5) ((fromIntegral screenHeight -565)) (scale 0.2 0.2 (color white(text "Press I to see the Instructions.")))
instructions = pictures [translate (110) ((fromIntegral screenHeight -100)) (scale 0.3 0.3 (color cellColor (text "INSTRUCTIONS"))),
						translate (10) ((fromIntegral screenHeight -180)) (scale 0.2 0.2 (color cellColor (text "Move tiles in grid to order them from 1 to 15."))),
						translate (10) ((fromIntegral screenHeight -220)) (scale 0.2 0.2 (color cellColor (text "To move a tile you can click on it or use  "))),
						translate (10) ((fromIntegral screenHeight -260)) (scale 0.2 0.2 (color cellColor (text "your arrow keys or W, A, S, D."))),
						translate (10) ((fromIntegral screenHeight -300)) (scale 0.2 0.2 (color cellColor (text "Press ENTER to continue to game.")))]
gameAsPicture :: Game -> Picture
gameAsPicture game = translate (fromIntegral screenWidth * (-0.5))
                               (fromIntegral screenHeight * (-0.5))
                               frame
    where frame = case gameState game of
						Running -> puzzleAsRunningPicture (puzzle game) (moves game)
						GameOver-> guiGameOver (moves game)
						Instructions -> instructions
 
getValoare :: Puzzle -> (Int,Int) -> Cell
getValoare puzzle (pos1, pos2) = puzzle!!pos1!!pos2

findPoz :: Puzzle ->Cell-> (Int,Int)
findPoz puzzle  x = head [ (i, j) | i <- [0 .. size - 1], j <- [0 .. size - 1], (getValoare puzzle (i, j)) == x]
    where
        size = length puzzle 