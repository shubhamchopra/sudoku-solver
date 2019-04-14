module Solver where

import qualified Data.Vector as V
import qualified Data.Set as Set

import Data.List (sortBy)

import Debug.Trace
import Printers

debug = flip trace

characterSet :: Set.Set Char
characterSet = Set.fromList "123456789" 

getRow :: Int -> V.Vector a -> Maybe (V.Vector a)
getRow i v
  | i >= 1 && i <= 9 = Just $ V.slice ((i-1)*9) 9 v
  | otherwise = Nothing

getColumn :: Int -> V.Vector b -> Maybe (V.Vector b)
getColumn i v
  | i >= 1 && i <= 9 = 
    let indices = V.generate 9 (\j -> 9*j + (i-1))
    in Just $ fmap (\j -> v V.! j) indices
  | otherwise = Nothing

getBlock :: Int -> V.Vector b -> Maybe (V.Vector b)
getBlock bId v
  | bId >= 1 && bId <= 9 = 
    let i = (bId - 1) `div` 3
        j = (bId - 1) `mod` 3
        indices = V.generate 9 (\id -> 3*9*i + 9*(id `div` 3)+ 3*j + (id `mod` 3))
     in Just $ fmap (\id -> v V.! id) indices
  | otherwise = Nothing     

getMissingNumbers :: Foldable t => t Char -> Set.Set Char
getMissingNumbers v = foldl (\s i -> if i /= '.' then Set.delete i s else s) characterSet v

checkIfVectorComplete :: V.Vector Char -> Bool
checkIfVectorComplete v = V.length v == 9 && (length $ getMissingNumbers v) == 0

getPotentialCandidates :: V.Vector Char -> Int -> Set.Set Char
getPotentialCandidates v pos 
  | char /= '.' = Set.singleton char
  | otherwise = 
    let rowId = (pos-1) `div` 9
        colId = (pos-1) `mod` 9
        Just row = getRow (rowId+1) v
        Just col = getColumn (colId+1) v
        Just block = getBlock ((rowId `div` 3)*3 + (colId `div` 3) + 1) v --`debug` (show pos ++ "row" ++ show rowId ++ "col" ++ show colId)
        missingRowNums = getMissingNumbers row --`debug` (prettyPrintBlock $ Just block)
        missingColNums = getMissingNumbers col 
        missingBlkNums = getMissingNumbers block
     in missingRowNums `Set.intersection` missingColNums `Set.intersection` missingBlkNums
  where char = v V.! (pos - 1)

getPositionWithFewestOptions ::
  V.Vector Char -> (Int, Set.Set Char)
getPositionWithFewestOptions v = 
  let openPositions = filter (\p -> v V.! p == '.') [0 .. 80]
      posOptions = map (\p -> (p+1, getPotentialCandidates v (p+1))) openPositions
      minOption = foldl1 (\(k1,v1) (k2,v2) -> if Set.size v1 < Set.size v2 then (k1, v1) else (k2, v2) ) posOptions
      --sortedOptions = sortBy (\(_,o1) (_,o2) -> compare (Set.size o1) (Set.size o2)) posOptions
   in if (length openPositions > 0 ) then minOption else (0, Set.empty)


--solve :: V.Vector Char -> Int -> [V.Vector Char]
--solve v 50 = 
--  --check for completeness
--  [v]
--solve v pos 
--  | char /= '.' = solve v (pos + 1)
--  | otherwise = 
--    let candidates = Set.toList $ getPotentialCandidates v pos --`debug` (prettyPrintPuzzle v)
--        recurSolve candidate = solve (V.update v (V.singleton (pos-1, candidate))) (pos+1)  
--     in concat $ map recurSolve candidates --`debug` ("pos: " ++ show pos ++ ", candidates: " ++ show candidates)
--  where char = v V.! (pos - 1)


solve :: V.Vector Char -> [V.Vector Char]
solve v = 
  let (pos, candidatesSet) = getPositionWithFewestOptions v --`debug` ("--------------------\n" ++ prettyPrintPuzzle v)
      candidates = Set.toList candidatesSet --`debug` ("pos: " ++ show pos ++ ", candidates: " ++ show candidatesSet)
      recurSolve c = solve (V.update v (V.singleton (pos-1, c)))
  in if pos == 0 then
     --when pos == 0, we have the solution            
       [v] 
     else 
       concat $ map recurSolve candidates
