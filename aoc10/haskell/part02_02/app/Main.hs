module Main where

import Prelude hiding (lookup)
import Debug.Trace (traceShowId)
import Debug.Trace (traceShow)
import Data.List (foldl')
import Data.List (sort)
import Data.List (group)
import Data.Char (digitToInt)
import Data.Map (Map)
import Data.Map (fromList)
import Data.Map (lookup)
import Data.Map (member)
import Data.Maybe (fromJust)
import qualified Data.Set as Set
import Data.Set (Set)

data Pipe = Vertical | Horizontal | NorthEast | NorthWest | SouthWest | SouthEast | Ground | Start deriving (Show, Eq)

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

startPipeType = NorthEast

connectionPoints :: Pipe -> [(Int, Int)]
connectionPoints Vertical = [(-1, 0), (1, 0)]
connectionPoints Horizontal = [(0, -1), (0, 1)]
connectionPoints NorthEast = [(-1, 0), (0, 1)]
connectionPoints NorthWest = [(-1, 0), (0, -1)]
connectionPoints SouthWest = [(1, 0), (0, -1)]
connectionPoints SouthEast = [(1, 0), (0, 1)]
connectionPoints Ground = []
connectionPoints Start = connectionPoints startPipeType

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

traversePipes :: PipeMap -> [(Int, Int)] -> (Int, Int) -> [(Int, Int)]
traversePipes pipeMap visited current = case nextPos of
      Nothing -> visited ++ [current]
      Just pos -> traversePipes pipeMap (visited ++ [current]) pos
  where
    nextPos = if length possibleSuccessors == 0 then Nothing else Just $ head possibleSuccessors
    possibleSuccessors = filter (\pos -> not $ elem pos visited) $ getConnectedPipes pipeMap current

floodFill :: PipeMap -> Set (Int, Int) -> (Int, Int) -> Set (Int, Int)
floodFill pipeMap filled pos@(row, column)
    | not pipeExists = filled
    | pipe /= Ground = filled
    | pos `Set.member` filled = filled
    | otherwise = foldl (\filled neighbor -> floodFill pipeMap filled neighbor) (Set.insert pos filled) neighbors
  where
    maybePipe = lookup pos pipeMap
    pipeExists = case maybePipe of
      Just _ -> True
      Nothing -> False
    pipe = fromJust maybePipe
    neighbors = [(row + 1, column), (row - 1, column), (row, column + 1), (row, column - 1)]

fillFromEdge :: (Int, Int) -> PipeMap -> Set (Int, Int) -> Set (Int, Int)
fillFromEdge (width, height) pipeMap filled = foldl (\filled pos -> floodFill pipeMap filled pos) filled positionsToCheck
  where
    positionsToCheck = concat [
      map (\column -> (0, column)) [0..width - 1],
      map (\column -> (height - 1, column)) [0..width - 1],
      map (\row -> (row, 0)) [0..height - 1],
      map (\row -> (row, width - 1)) [0..height - 1]]

replace :: Char -> String -> String -> String
replace _ _ "" = ""
replace what vomit (x:xs)
  | x == what = vomit ++ replace what vomit xs
  | otherwise = (x:replace what vomit xs)

transformInput :: [String] -> [String]
transformInput input = concat $ map (\s -> [s, transformCopy s]) $ map transformLine input
  where
    transformLine "" = ""
    transformLine (x:xs)
      | x == '|' = "|." ++ transformLine xs
      | x == '-' = "--" ++ transformLine xs
      | x == 'L' = "L-" ++ transformLine xs
      | x == 'J' = "J." ++ transformLine xs
      | x == '7' = "7." ++ transformLine xs
      | x == 'F' = "F-" ++ transformLine xs
      | x == '.' = ".." ++ transformLine xs
      | x == 'S' = "S-" ++ transformLine xs
    transformCopy "" = ""
    transformCopy (x:xs)
      | x == '-' = ('.':transformCopy xs)
      | x == 'L' = ('.':transformCopy xs)
      | x == 'J' = ('.':transformCopy xs)
      | x == '7' = ('|':transformCopy xs)
      | x == 'F' = ('|':transformCopy xs)
      | x == 'S' = ('|':transformCopy xs)
      | otherwise = (x:transformCopy xs)

determineStart :: [String] -> (Int, Int)
determineStart input = (startRow, startColumn)
  where
    (startRow, startRowContents) = head $ filter (elem 'S' . snd) $ zip [0..] $ input
    startColumn = fst $ head $ filter ((=='S') . snd) $ zip [0..] startRowContents

determineDimensions :: [String] -> (Int, Int)
determineDimensions input = (length $ head input, length input)

--calculate :: (Int, Int) -> [(Int, Int)] -> Set (Int, Int) -> Int
--calculate (width, height) route filled = length $ filter (\pos -> ((not $ Set.member pos filled) && (not $ elem pos route))) $ traceShow ("num possibilities: " ++ show (length possiblePositions)) $ possiblePositions
--  where
--    possiblePositions = [(row, column) | column <- [0..width - 1], row <- [0..height - 1], column `mod` 2 == 0 && row `mod` 2 == 0]

--processLine :: Int -> String -> Int
--processLine _ "" = 0
--processLine True (x:xs) =
--processLine False (x:xs) =

--calculate :: (Int, Int) -> [(Int, Int)] -> Int
--calculate (width, height) route = sum $ map (\row -> traceShow ("row: " ++ show row) $ traceShowId $ processRow row) [0..1]
--  where
--    isOccupied pos = elem pos route
--
--    processRow :: Int -> Int
--    processRow row = fst $ foldl (\(acc, opened) occupied -> traceShow ("opened: " ++ show opened ++ " occupied: " ++ show occupied) $ (acc + if opened && not occupied then traceShow "adding" 1 else 0, openedNext opened occupied)) (0, False) $ map (traceShowId . isOccupied . (\column -> (row, column))) [0..width - 1]
--
--    openedNext opened occupied = case (opened, occupied) of
--      (True, True) -> True
--      (True, False) -> True
--      ()


main :: IO ()
main = do
  contents <- readFile "data.txt"
--  let input = transformInput $ lines contents
  let input = lines contents
  let start = determineStart input
--  putStrLn $ "start " ++ show start
  let pipeMap = createMap $ map (map asPipe) $ input
  let route = (traversePipes pipeMap [] start)
  print route
--  let dimensions = determineDimensions input
--  let edgeFilled = fillFromEdge dimensions pipeMap Set.empty
--  let numFilled = length edgeFilled
--  let onlyEvens = Set.filter (\(row, column) -> row `mod` 2 == 0 && column `mod` 2 == 0) edgeFilled

--  print onlyEvens
--  print $ length onlyEvens
--
--  putStrLn $ "dimensions: " ++ show dimensions

--  print $ calculate dimensions route onlyEvens

--  print $ [(row, column) | column <- [0..10 - 1], row <- [0..10 - 1], column `mod` 2 == 0 && row `mod` 2 == 0, not $ elem (row, column) route]
--  let result = calculate dimensions route
--  print result
