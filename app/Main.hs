module Main where

import System.Environment (getArgs)
import qualified Data.Vector as V
import Control.Monad

import Solver
import Printers
import AlgoXSolver

main :: IO ()
main = do
  lines <- fmap lines $ readFile "sudoku17.10.txt"
  let vectors = fmap V.fromList lines
  putStrLn $ "Total puzzles read: " ++ (show $ length vectors)
  forM_ vectors $ \v -> do
    putStrLn "Puzzle"
    putStrLn $ prettyPrintPuzzle v
    putStrLn "Tree traversal solution..."
--    printSolution $ solve v
    putStrLn "Algo X solution..."
    printSolution $ algoXSudokuSolve v


printSolution :: Show a => [V.Vector a] -> IO ()
printSolution solutions =
  case solutions of 
    (s:_) -> putStrLn $ prettyPrintPuzzle s
    [] -> putStrLn "No solution"
    
