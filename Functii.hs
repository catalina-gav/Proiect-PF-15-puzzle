module Functii where
import Game
--afisez valoarea pentru o pozitie data din matrice
getValoare :: Puzzle -> Poz -> Cell
getValoare puzzle (pos1, pos2) = puzzle!!pos1!!pos2
--afisez pozitia pentru valoarea data

findPoz :: Puzzle ->Cell-> Poz
findPoz puzzle  x = head [ (i, j) | i <- [0 .. size - 1], j <- [0 .. size - 1], (getValoare puzzle (i, j)) == x]
    where
        size = length puzzle

--i si j pentru valoarea 0 -blank din matrice 
firstZero :: Puzzle -> Int
firstZero puzzle = fst(findPoz puzzle 0)
secondZero :: Puzzle -> Int
secondZero puzzle = snd(findPoz puzzle 0)
--vecinii pentru o pozitie
availablePozToMove :: Poz -> [Poz]
availablePozToMove (pos1,pos2) = [ (pos1 - 1, pos2), (pos1, pos2-1), (pos1 + 1, pos2), (pos1, pos2 + 1) ]
--vecinii pentru pozitia blank
availableZero :: Puzzle -> [Poz]
availableZero puzzle = availablePozToMove (firstZero puzzle , secondZero puzzle)

--interschimb 2 valori din matrice (avand pozitiile lor ca parametri)
swapPuzzleValues :: Puzzle -> Poz -> Poz -> Puzzle
swapPuzzleValues puzzle pos1 pos2 = changeMatrixValue pos1 (changeMatrixValue pos2 puzzle val1) val2
    where
        val1 = getValoare puzzle pos1
        val2 = getValoare puzzle pos2
--comanda data de jucator
move :: Cell -> Puzzle ->Puzzle
move number puzzle= swapPuzzleValues (puzzle) ( findPoz puzzle number) (firstZero puzzle,secondZero puzzle) 
--functie pentru a schimba valoarea unui element din lista la pozitia data
changeListValue :: Int -> [a] -> (a -> a) -> [a]
changeListValue _ [] _ = []
changeListValue 0 (x:xs) f = (f x) : xs
changeListValue pos (x:xs) f = x : changeListValue (pos - 1) xs f

--schimbarea valorii pentru elementul de pe poz (i,j) din matrice
changeMatrixValue :: Poz -> [[a]] -> a -> [[a]]
changeMatrixValue (pos1, pos2) matrix val = changeListValue pos1 matrix (\row -> changeListValue pos2 row (const val))



