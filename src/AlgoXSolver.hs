{-# LANGUAGE DeriveGeneric #-}
module AlgoXSolver where

import GHC.Generics
import Data.List (groupBy)
import qualified Data.HashSet as Set
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as Map
import Data.Hashable

import Printers
import Debug.Trace

debug = flip trace

data ConstraintRow = ConstraintRow {rowId:: Int, colId:: Int, num:: Char}
  deriving (Eq, Ord, Show, Generic)

instance Hashable ConstraintRow

type ConstraintCol = String

data ConstraintMatrix = ConstraintMatrix { matrix:: Set.HashSet (ConstraintRow, ConstraintCol)
                                         , constraints:: Set.HashSet ConstraintCol
                                         } deriving Show

charSet :: [Char]
charSet = "123456789"

createRowColumnConstraints :: [(ConstraintRow, ConstraintCol)]
createRowColumnConstraints = 
  [ (ConstraintRow i j c, "R" ++ show i ++ "C" ++ show j)| i <- [1 .. 9], j <- [1 .. 9], c <- charSet]

createRowConstraints :: [(ConstraintRow, ConstraintCol)]
createRowConstraints = 
  [ (ConstraintRow i j c, "R" ++ show i ++ "#" ++ show c) | i <- [1 .. 9], c <- charSet, j <- [1 .. 9]]

createColConstraints :: [(ConstraintRow, ConstraintCol)]
createColConstraints = 
  [ (ConstraintRow i j c, "C" ++ show j ++ "#" ++ show c) | j <- [1 .. 9], c <- charSet, i <- [1 .. 9]]

getBoxRowCol :: Integral b => b -> [(b, b)]
getBoxRowCol boxId = 
  [(((boxId-1) `div` 3)*3 + i, ((boxId-1) `mod` 3)*3 + j) | i <- [1..3], j <-[1..3]]

createBoxConstraints :: [(ConstraintRow, ConstraintCol)]
createBoxConstraints = 
  [ (ConstraintRow i j c, "B" ++ show b ++ "#" ++ show c) | b <- [1 .. 9], c <- charSet, (i, j) <- getBoxRowCol b]

createConstraintMatrix :: ConstraintMatrix
createConstraintMatrix = 
  let matrix = (Set.fromList createRowColumnConstraints) `Set.union` (Set.fromList createRowConstraints) `Set.union` (Set.fromList createColConstraints) `Set.union` (Set.fromList createBoxConstraints)
      constraints = Set.map snd matrix
   in ConstraintMatrix matrix constraints

getRowColId :: Integral b => b -> (b, b)
getRowColId pos = ((pos `div` 9) + 1, (pos `mod` 9) + 1)

getPos :: Num a => a -> a -> a
getPos row col = (row-1) * 9 + (col - 1)

removeConstraints ::
  ConstraintMatrix -> ConstraintRow -> ConstraintMatrix
removeConstraints (ConstraintMatrix constMatrix constSet) constRow  = 
  let row = Set.filter (\(cr, _) -> cr == constRow) constMatrix
      satisfiedConstraints = Set.map snd row
      colsToRemove = Set.filter (\(_, const) -> Set.member const satisfiedConstraints) constMatrix
      colsToRemoveRowIds = Set.map fst colsToRemove
      relatedRows = Set.filter (\(cr, _) -> Set.member cr colsToRemoveRowIds) constMatrix
      elemsToRemove = row `Set.union` colsToRemove `Set.union` relatedRows
   in ConstraintMatrix (constMatrix `Set.difference` elemsToRemove) (constSet `Set.difference` satisfiedConstraints)

createSudokuConstraintMatrix :: V.Vector Char -> ConstraintMatrix
createSudokuConstraintMatrix v = 
  let f constMatrix (idx, c) = removeConstraints constMatrix $ ConstraintRow rowId colId c where (rowId, colId) = getRowColId idx
      currentChars = V.filter (\(_,c) -> c /= '.') (V.indexed v)
   in V.foldl f createConstraintMatrix currentChars

getConstraintWithMinOptions ::
  (Ord a, Num a) => ConstraintMatrix -> (ConstraintCol, a)
getConstraintWithMinOptions (ConstraintMatrix cMatrix cSet) = 
  let constPossCount = Set.foldr (\(_, const) m -> Map.insert const ((Map.lookupDefault 0 const m)+1) m) Map.empty cMatrix
   in if Map.size constPossCount == Set.size cSet
         then
         -- all constraints still have viable possibilites, iterate through the map and get one with lowest number of possibilites
         let countList = Map.toList constPossCount
          in foldl1 (\(k1,v1) (k2, v2) -> if v1 < v2 then (k1, v1) else (k2, v2)) countList
      else
          ([], 0)
--         (Set.elemAt 0 $ cSet `Set.difference` (Map.keysSet constPossCount), 0)
         
algoXSolve ::
  ConstraintMatrix -> [ConstraintRow] -> [[ConstraintRow]]
algoXSolve cm@(ConstraintMatrix constMatrix constSet) currentSolution = 
  if Set.size constSet == 0
     then
     -- we have our solution since all constraints are met
     [currentSolution]
  else
    let (constraint, optionCount) = getConstraintWithMinOptions cm
     in if optionCount == 0
           then [] --`debug` ("backtracking: option count: " ++ show optionCount)
        else
           let options = map fst $ Set.toList $ Set.filter (\(_, col) -> col == constraint) constMatrix
               f option = algoXSolve (removeConstraints cm option) (option:currentSolution) --`debug` ("in map option: " ++ show option)
            in concatMap f options --`debug` (show options)

algoXSudokuSolve :: V.Vector Char -> [V.Vector Char]
algoXSudokuSolve v = 
  let constMatrix = createSudokuConstraintMatrix v --`debug` ("starting puzzle\n" ++ prettyPrintPuzzle v)
      solutions = algoXSolve constMatrix [] --`debug` (show constMatrix)
      f solution = V.fromList $ map (\(ConstraintRow rowId colId c) -> (getPos rowId colId, c)) solution
   in map (V.update v . f) solutions


