module Main where

import Debug.Trace (traceShowId)
import Data.List (foldl')

solve :: Int -> Int -> (Int, Int)
solve time distance = (ceiling (-p/2 - discriminantRoot + 0.000000001), floor (-p/2 + discriminantRoot - 0.000000001))
  where
    p = fromIntegral (-time)
    q = fromIntegral distance
    discriminantRoot = sqrt $ (-p/2) * (-p/2) - q

main :: IO ()
main = do
  contents <- readFile "data.txt"
  let [times, distances] = map (map (read :: String -> Int) . tail . words) $ lines contents
  let races = zip times distances
  let solutions = foldl' (*) 1 $ map ((\(low, high) -> high - low + 1) . (\(time, distance) -> solve time distance)) races
  print solutions
