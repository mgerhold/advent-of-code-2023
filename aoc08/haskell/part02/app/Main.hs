module Main where

import Prelude hiding (lookup)
import Debug.Trace (traceShowId)
import Data.List (foldl')
import Data.List (sort)
import Data.List (group)
import Data.List (elemIndex)
import Data.Char (digitToInt)
import Data.Map (Map)
import Data.Map (fromList)
import Data.Map (lookup)
import Data.Maybe (fromJust)
import qualified Data.Set as Set

type NodeMap = (Map String (String, String))

proceed :: NodeMap -> [String] -> Char -> [String]
proceed nodeMap currentNodes step = map successor currentNodes
  where
      next origin = fromJust $ lookup origin nodeMap
      left origin = fst $ next origin
      right origin = snd $ next origin
      successor origin = (if step == 'L' then left else right) origin

numSteps :: NodeMap -> [String] -> String -> Int
numSteps nodeMap startingNodes (x:xs)
  | all ((=='Z') . last) startingNodes = 0
  | otherwise = 1 + numSteps nodeMap (proceed nodeMap startingNodes x) xs

successor :: NodeMap -> String -> Char -> String
successor nodeMap from step = if step == 'L' then left else right
  where
    next = fromJust $ lookup from nodeMap
    left = fst next
    right = snd next

nodeSequence :: NodeMap -> String -> String -> [String]
nodeSequence nodeMap (x:xs) node = (nextNode:nodeSequence nodeMap xs nextNode)
  where
    nextNode = successor nodeMap node x

completeSequence :: NodeMap -> String -> String -> [String]
completeSequence nodeMap navigation node = (node:nodeSequence nodeMap navigation node)

getCycle :: Int -> [String] -> [String] -> ([String], [String])
getCycle cycleLength visited (x:xs)
    | (elem x visited && ((length visited - index) `mod` cycleLength == 0)) = (drop index visited, visited)
    | otherwise = getCycle cycleLength (visited ++ [x]) xs
  where
    index = fromJust $ elemIndex x visited

indexOfFirstEndingZ :: [String] -> Int
indexOfFirstEndingZ (x:xs)
    | endsWithZ x = 0
    | otherwise = 1 + indexOfFirstEndingZ xs
  where
    endsWithZ s = (== 'Z') $ last s

main :: IO ()
main = do
  contents <- readFile "data.txt"
  let rows = filter (not . null) $ lines contents
  let navigation = concat $ repeat $ head rows
  let nodes = map ((\[from, toLeft, toRight] -> (from, (toLeft, toRight))) . words . (filter (\c -> not (elem c "=(),")))) (tail rows)
  let startingNodes = filter ((=='A') . last) $ map (fst) nodes
  let nodeMap = fromList nodes
  let cycles = map (\node -> getCycle (length $ head rows) [] $ completeSequence nodeMap navigation node) startingNodes
  print $ foldl lcm 1 $ map (length . fst) cycles
