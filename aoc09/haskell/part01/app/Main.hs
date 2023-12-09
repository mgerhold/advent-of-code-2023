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

predict :: [Int] -> Int
predict list
  | all (== 0) list = 0
  | otherwise = last list + (predict $ zipWith (-) (tail list) list)

postdict :: [Int] -> Int
postdict list
  | all (== 0) list = 0
  | otherwise = head list - (postdict $ zipWith (-) (tail list) list)

main :: IO ()
main = do
  contents <- readFile "data.txt"
  let histories = map (map (read :: (String -> Int)) . words) $ lines contents
  let predictions = map predict histories
  let postdictions = map postdict histories
  print $ "part 1: " ++ (show $ sum predictions)
  print $ "part 2: " ++ (show $ sum postdictions)
