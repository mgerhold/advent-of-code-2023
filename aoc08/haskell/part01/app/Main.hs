module Main where

import Prelude hiding (lookup)
import Debug.Trace (traceShowId)
import Data.List (foldl')
import Data.List (sort)
import Data.List (group)
import Data.Char (digitToInt)
import Data.Map (Map)
import Data.Map (fromList)
import Data.Map (lookup)
import Data.Maybe (fromJust)

numSteps :: (Map String (String, String)) -> String -> String -> Int
numSteps _ start _ | start == "ZZZ" = 0
numSteps nodes start (x:xs)
    | x == 'R' = 1 + numSteps nodes right xs
    | x == 'L' = 1 + numSteps nodes left xs
  where
    next = fromJust $ lookup start nodes
    left = fst next
    right = snd next

main :: IO ()
main = do
  contents <- readFile "data.txt"
  let rows = filter (not . null) $ lines contents
  let navigation = concat $ repeat $ head rows
  let nodes = fromList $ map ((\[from, toLeft, toRight] -> (from, (toLeft, toRight))) . words . (filter (\c -> not (elem c "=(),")))) (tail rows)
  let result = numSteps nodes "AAA" navigation
  print result
