module Rendering where
import Graphics.Gloss
import Game
import Functii

bgColor = mixColors  95 5 black cyan 
puzzleOutline = mixColors 80 20 black bgColor
tileColor = mixColors 70 30 white bgColor
-- translatez fiecare text in celula lui din tabel, desenez dreptunghiurile albe din spatele numerelor
numberToTile :: Picture -> (Int,Int) -> Picture
numberToTile picture (row, column) = pictures [(translate z t (color tileColor $ rectangleSolid  (tileWidth-15)(tileHeight-15))),(translate x y picture)]
    where 
			x = fromIntegral column * tileWidth + tileWidth * 0.35
			y = fromIntegral row * tileHeight + tileHeight * 0.35
			z = fromIntegral column * tileWidth + tileWidth * 0.5
			t = fromIntegral row * tileHeight + tileHeight * 0.5
-- ac lucru dar fiind zero nu mai desenez un dreptunghi in spate			  
numberToTileZero :: Picture -> (Int,Int) -> Picture
numberToTileZero picture (row, column) = (translate x y picture)
    where 
			x = fromIntegral column * tileWidth + tileWidth * 0.06
			y = fromIntegral row * tileHeight + tileHeight * 0.06
		
--transform numerele in picturi
printNumber :: String -> Picture
printNumber "0" = scale 0.5 0.5 (text " ")
printNumber s = scale 0.5 0.5 (text s)
--desenez tabla de joc
puzzleTiles :: List -> Puzzle -> [Picture] 
puzzleTiles  [] _ = []
puzzleTiles (0:xs) puzzle = (numberToTileZero (printNumber (show 0)) (findPoz puzzle 0)) : (puzzleTiles xs puzzle)
puzzleTiles (x:xs) puzzle = (numberToTile (printNumber (show x)) (findPoz puzzle x)) : (puzzleTiles xs puzzle)

tilesToPicture :: Puzzle -> Picture
tilesToPicture puzzle = pictures ( puzzleTiles (concat puzzle) puzzle )
--desenul pentru starea de Running
guiRunning :: Puzzle -> Int -> Picture
guiRunning puzzle score =
    pictures [ translate (tileWidth+tileWidth) (tileHeight +tileHeight) (color puzzleOutline $ rectangleSolid (fromIntegral width +20) (fromIntegral height +20))
				,color black $ tilesToPicture puzzle
			  , guiScore score
			  , restartGame
			  , newGame
			  ,gameInstructions
             ]
--desenul pentru starea de Game Over
guiGameOver :: Int->Picture
guiGameOver score = pictures [
						translate (110) (fromIntegral height -100) (scale 0.3 0.3 (color tileColor (text("MOVES: " ++ (show score))))),
						translate (10) (fromIntegral height -180) (scale 0.3 0.3 (color tileColor (text "You solved the 15-Puzzle!"))),
						translate (10) (fromIntegral height -220) (scale 0.2 0.2 (color tileColor (text "Press R to restart game."))),
						translate (10) (fromIntegral height -260) (scale 0.2 0.2 (color tileColor (text "Press N to start New Game."))),
						translate (10) (fromIntegral height -300) (scale 0.2 0.2 (color tileColor (text "Press I to see the Instructions.")))
						]
--afisarea scorului					
guiScore :: Int->Picture 
guiScore  score = translate (110) ((fromIntegral height +20)) (scale 0.3 0.3 $ color white $ text("MOVES: " ++ (show score)))
--afisarea comenzilor pe care le poate efectua jucatorul
restartGame :: Picture
restartGame =translate (5) ((fromIntegral height -515)) (scale 0.2 0.2 $ color white $ text "Press R to restart game." )

newGame :: Picture
newGame =translate (5) ((fromIntegral height -540)) (scale 0.2 0.2 $ color white $ text "Press N to start New Game." )

gameInstructions :: Picture
gameInstructions = translate (5) ((fromIntegral height -565)) (scale 0.2 0.2  $ color white $ text "Press I to see the Instructions.")
--desenul pentru starea jocului legata de instructiuni
guiInstructions = pictures [
							translate (110) ((fromIntegral height -100)) (scale 0.3 0.3 $ color tileColor $ text "INSTRUCTIONS"),
							translate (10) ((fromIntegral height -180)) (scale 0.2 0.2  $ color tileColor $ text "Move tiles in grid to order them from 1 to 15."),
							translate (10) ((fromIntegral height -220)) (scale 0.2 0.2 $ color tileColor $ text "To move a tile you can click on it or use  "),
							translate (10) ((fromIntegral height -260)) (scale 0.2 0.2 $ color tileColor $ text "your arrow keys or W, A, S, D."),
							translate (10) ((fromIntegral height -300)) (scale 0.2 0.2 $ color tileColor $ text "Press ENTER to continue to game.")
						]
--desenarea efectiva a jocului pe ecran in functie de starea acestuia
gameAsPicture :: Game -> Picture
gameAsPicture game = translate (fromIntegral width * (-0.5))
                               (fromIntegral height * (-0.5))
                               frame
    where frame = case gameState game of
						Running -> guiRunning (puzzle game) (moves game)
						GameOver-> guiGameOver (moves game)
						Instructions -> guiInstructions
 
