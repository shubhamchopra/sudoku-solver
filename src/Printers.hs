module Printers (
                  prettyPrintPuzzle
                , prettyPrintBlock
                ) where

import qualified Data.Vector as V

prettyPrint :: Show a => Int -> V.Vector a -> String
prettyPrint size vector = 
  let vectors = fmap (\i -> V.slice (size*i) size vector) [0 .. (size - 1)]
      strVectors = map show vectors
   in unlines strVectors

prettyPrintPuzzle :: Show a => V.Vector a -> String
prettyPrintPuzzle vector = prettyPrint 9 vector

prettyPrintBlock :: Show a => Maybe (V.Vector a) -> String
prettyPrintBlock (Just vector) = prettyPrint 3 vector
prettyPrintBlock _ = ""

