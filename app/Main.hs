module Main where

main :: IO ()
main = print $ getSolvedList $ solvePuzzle $ PartialSolve [1..81]

newtype PartialSolve = PartialSolve [Int]
newtype Solved = Solved PartialSolve

solvePuzzle :: PartialSolve -> Solved
solvePuzzle sudoku =
    if elem 0 (getList sudoku)
        then solvePuzzle $ addEntry sudoku
        else Solved sudoku

addEntry :: PartialSolve -> PartialSolve
addEntry (PartialSolve (0:xs)) = PartialSolve (1:xs)
addEntry (PartialSolve (x:xs)) =
    PartialSolve (x:(getList $ addEntry $ PartialSolve xs))

getList :: PartialSolve -> [Int]
getList (PartialSolve xs) = xs

getSolvedList :: Solved -> [Int]
getSolvedList (Solved (PartialSolve xs)) = xs
