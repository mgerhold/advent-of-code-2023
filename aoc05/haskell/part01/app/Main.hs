module Main where

import Debug.Trace (traceShowId)
import Data.List.Split (splitWhen)
import Data.Function ((&))

toIntList :: String -> [Int]
toIntList = map read . words

applyMapping :: [(Int, Int, Int)] -> Int -> Int
applyMapping [] value = value
applyMapping ((dest_start, source_start, length):xs) value
  | value >= source_start && value < source_start + length = dest_start + value - source_start
  | otherwise = applyMapping xs value

main :: IO ()
main = do
  contents <- readFile "data.txt"
  let blocks = splitWhen null (lines contents)
  let seeds = toIntList $ (splitWhen (== ':') (head $ head blocks))!!1
  let mappings = map ((map ((\[dest_start, source_start, length] -> (dest_start, source_start, length)) . toIntList)) . tail) $ tail blocks
  let mappingFunctions = map applyMapping mappings
  let mappedSeeds = map (flip (foldl (&)) mappingFunctions) seeds
  let minLocation = foldl min (head mappedSeeds) (tail mappedSeeds)
  print $ minLocation
