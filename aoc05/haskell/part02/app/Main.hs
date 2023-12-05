module Main where

import Data.List.Split (splitWhen)
import Data.List.Split (chunksOf)
import Data.List (sort)

toIntList :: String -> [Int]
toIntList = map read . words

cutOut :: [(Int, Int, Int)] -> (Int, Int) -> [(Int, Int)]
cutOut [] range = [range]
cutOut (mapping@(destStart, sourceStart, length):xs) (start, end)
  | end <= sourceStart || start >= sourceEnd = cutOut xs (start, end)
  | start >= sourceStart && end <= sourceEnd = [(destStart + start - sourceStart, destStart - sourceStart + end)]
  | start < sourceStart && end > sourceEnd = (cutOut xs (start, sourceStart)) ++ (cutOut [mapping] (sourceStart, sourceEnd)) ++ (cutOut xs (sourceEnd, end))
  | start < sourceStart && end <= sourceEnd = (cutOut xs (start, sourceStart)) ++ (cutOut [mapping] (sourceStart, end))
  | start >= sourceStart && end > sourceEnd = (cutOut [mapping] (start, sourceEnd)) ++ (cutOut xs (sourceEnd, end))
    where
      sourceEnd = sourceStart + length

main :: IO ()
main = do
  contents <- readFile "data.txt"
  let blocks = splitWhen null (lines contents)
  let seeds = map (\[a, b] -> (a, a + b)) $ chunksOf 2 $ toIntList $ (splitWhen (== ':') (head $ head blocks))!!1
  let mappings = map ((map ((\[destStart, sourceStart, length] -> (destStart, sourceStart, length)) . toIntList)) . tail) $ tail blocks
  let applyAllMapping = applyFunctions $ map cutOut mappings where
      applyFunctions [] tuple = [tuple]
      applyFunctions (x:xs) tuple = concat $ map (applyFunctions xs) (x tuple)

  print $ fst $ minimum $ concat $ map applyAllMapping seeds
