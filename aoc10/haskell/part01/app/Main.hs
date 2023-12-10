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
import Data.Map (member)
import Data.Maybe (fromJust)

data Pipe = Vertical | Horizontal | NorthEast | NorthWest | SouthWest | SouthEast | Ground | Start deriving Show

asPipe :: Char -> Pipe
asPipe '|' = Vertical
asPipe '-' = Horizontal
asPipe 'L' = NorthEast
asPipe 'J' = NorthWest
asPipe '7' = SouthWest
asPipe 'F' = SouthEast
asPipe '.' = Ground
asPipe 'S' = Start

type PipeMap = Map (Int, Int) Pipe

createMap :: [[Pipe]] -> PipeMap
createMap allPipes = fromList $ concat $ [createRow row rowPipes | (row, rowPipes) <- zip [0..] allPipes]
  where
    createRow :: Int -> [Pipe] -> [((Int, Int), Pipe)]
    createRow row pipes = [((row, column), pipe) | (column, pipe) <- zip [0..] pipes]

connectionPoints :: Pipe -> [(Int, Int)]
connectionPoints Vertical = [(-1, 0), (1, 0)]
connectionPoints Horizontal = [(0, -1), (0, 1)]
connectionPoints NorthEast = [(-1, 0), (0, 1)]
connectionPoints NorthWest = [(-1, 0), (0, -1)]
connectionPoints SouthWest = [(1, 0), (0, -1)]
connectionPoints SouthEast = [(1, 0), (0, 1)]
connectionPoints Ground = []
connectionPoints Start = connectionPoints SouthEast

lookupPipe :: (Int, Int) -> PipeMap -> Pipe
lookupPipe pos pipeMap = case lookup pos pipeMap of
  Just pipe -> pipe
  Nothing -> Ground

getConnectedPipes :: PipeMap -> (Int, Int) -> [(Int, Int)]
getConnectedPipes pipeMap pos@(row, column) = filter isConnected [(row, column + 1), (row, column - 1), (row - 1, column), (row + 1, column)]
  where
    isConnected :: (Int, Int) -> Bool
    isConnected otherPos = otherPos `elem` myConnections && pos `elem` (yourConnections otherPos)

    myPipe :: Pipe
    myPipe = lookupPipe pos pipeMap

    myConnections :: [(Int, Int)]
    myConnections = connections pos myPipe

    yourPipe :: (Int, Int) -> Pipe
    yourPipe otherPos = lookupPipe otherPos pipeMap

    connections :: (Int, Int) -> Pipe -> [(Int, Int)]
    connections (row, column) pipe = map (\(r, c) -> (row + r, column + c)) $ connectionPoints pipe

    yourConnections :: (Int, Int) -> [(Int, Int)]
    yourConnections yourPos = connections yourPos (yourPipe yourPos)

traversePipes :: PipeMap -> [(Int, Int)] -> (Int, Int) -> Int
traversePipes pipeMap visited current = case nextPos of
      Nothing -> 1
      Just pos -> 1 + (traversePipes pipeMap (current:visited) pos)
  where
    nextPos = if length possibleSuccessors == 0 then Nothing else Just $ head possibleSuccessors
    possibleSuccessors = filter (\pos -> not $ elem pos visited) $ getConnectedPipes pipeMap current

main :: IO ()
main = do
  contents <- readFile "data.txt"
  let (startRow, startRowContents) = head $ filter (elem 'S' . snd) $ zip [0..] $ lines contents
  let startColumn = fst $ head $ filter ((=='S') . snd) $ zip [0..] startRowContents
  let start = (startRow, startColumn)
  print start
  let pipeMap = createMap $ map (map asPipe) $ lines contents
  let connected = getConnectedPipes pipeMap (3, 1)
  let result = (traversePipes pipeMap [] start) `div` 2
  print result
