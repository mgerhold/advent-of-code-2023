module Main where

import Debug.Trace (traceShowId)
import Data.List (foldl')
import Data.Ix (rangeSize)

solve :: Int -> Int -> (Int, Int)
solve time distance = (ceiling (-p/2 - discriminantRoot + 0.000000001), floor (-p/2 + discriminantRoot - 0.000000001))
  where
    p = fromIntegral (-time)
    q = fromIntegral distance
    discriminantRoot = sqrt $ (-p/2) * (-p/2) - q

main :: IO ()
main = do
  solution <- rangeSize . (uncurry solve) . (\[a, b] -> (a, b)) . map (read . concat . tail . words) . lines <$> (readFile "data.txt")
  print solution
