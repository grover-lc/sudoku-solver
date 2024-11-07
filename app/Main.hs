module Main where

import Control.Monad (replicateM)
import Data.Array


-- define program flow

main :: IO ()
main = do
    putStrLn ""
    sudokuPuzzleStr <- collectProblem
    let sudokuSolvedStr = solvePuzzleStr sudokuPuzzleStr
    putStrLn "\nSolution:"
    putStrLn $ unlines sudokuSolvedStr

collectProblem :: IO [String]
collectProblem = do
    putStrLn "Input Sudoku puzzle:"
    theLines <- replicateM 9 getLine
    if all isValidLine theLines
        then return $ map completeLine theLines
        else do
            putStrLn
                ("\nLines must be exactly 9 characters long, \
                \consisting only of spaces and the digits 1 through 9."
                )
            collectProblem


-- string tools for program flow

isValidLine :: String -> Bool
isValidLine str = (hasCorrectLength str) && (hasCorrectChars str)

hasCorrectLength :: String -> Bool
hasCorrectLength = (>=) 9 . length

hasCorrectChars :: String -> Bool
hasCorrectChars = all (\x -> elem x " 123456789")

completeLine :: String -> String
completeLine str = str ++ replicate (9 - length str) ' '


-- sudoku operations

newtype Sudoku = Sudoku (Array (Int, Int) Char)
    deriving Eq

getArrayS :: Sudoku -> Array (Int, Int) Char
getArrayS (Sudoku sudokuArray) = sudokuArray

listSudoku :: [String] -> Sudoku
listSudoku sudokuStr = Sudoku $ arrayBuilder
    [((i, j), (sudokuStr !! (i-1)) !! (j-1)) | i <- [1..9], j <- [1..9]]

sudokuList :: Sudoku -> [String]
sudokuList sudoku = [sudokuString i sudoku | i <- [1..9]]

sudokuString :: Int -> Sudoku -> String
sudokuString i sudoku = [getArrayS sudoku ! (i, j) | j <- [1..9]]

solvePuzzle :: Sudoku -> Sudoku
solvePuzzle sudoku =
    if (isFinished sudoku) || (makeProgress sudoku == sudoku)
        then sudoku
        else solvePuzzle $ makeProgress sudoku

solvePuzzleStr :: [String] -> [String]
solvePuzzleStr = sudokuList . solvePuzzle . listSudoku

makeProgress :: Sudoku -> Sudoku
makeProgress = candidatesSudoku . sudokuCandidates

isFinished :: Sudoku -> Bool
isFinished sudoku = all ((/=) ' ') (getArrayS sudoku)


-- candidates

newtype Candidates = Candidates (Array (Int, Int) [Char])
    deriving Eq

getArrayC :: Candidates -> Array (Int, Int) [Char]
getArrayC (Candidates cands) = cands

sudokuCandidates :: Sudoku -> Candidates
sudokuCandidates sudoku = Candidates $ arrayBuilder
    [((i, j), positionToSet sudoku (i, j))
    | i <- [1..9], j <- [1..9]]

positionToSet :: Sudoku -> (Int, Int) -> [Char]
positionToSet sudoku (i, j) =
    case getArrayS sudoku ! (i, j) of
        ' ' ->
            [char
            | char <- "123456789"
            , notInRow sudoku char i
            , notInCol sudoku char j
            , notInBlock sudoku char (i, j)
            ]
        char -> [char]

notInRow :: Sudoku -> Char -> Int -> Bool
notInRow sudoku char i =
    char `notElem` [getArrayS sudoku ! (i, j) | j <- [1..9]]

notInCol :: Sudoku -> Char -> Int -> Bool
notInCol sudoku char j =
    char `notElem` [getArrayS sudoku ! (i, j) | i <- [1..9]]

notInBlock :: Sudoku -> Char -> (Int, Int) -> Bool
notInBlock sudoku char (i, j) =
    let (id3, jd3) = ((i-1) `div` 3, (j-1) `div` 3) in
        char `notElem`
        [getArrayS sudoku ! (k,l)
        | k <- [3*id3+1..3*id3+3], l <- [3*jd3+1..3*jd3+3]]

candidatesSudoku :: Candidates -> Sudoku
candidatesSudoku cands = Sudoku $ arrayBuilder
    [((i, j), pickSingletons (getArrayC cands ! (i, j)))
    | i <- [1..9], j <- [1..9]]

pickSingletons :: [Char] -> Char
pickSingletons chars =
    case chars of
        [] -> 'e'
        [char] -> char
        _ -> ' '


-- tools

arrayBuilder :: [((Int, Int), a)] -> Array (Int, Int) a
arrayBuilder = array ((1,1), (9,9))
