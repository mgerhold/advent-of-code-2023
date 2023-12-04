module Main where

-- Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53

import Debug.Trace (traceShowId)
import Debug.Trace (trace)
import Data.Ix (range)
import qualified Data.Map as Map

data Card = Card Int [Int] [Int] deriving Show

splitOnce :: Char -> String -> [String]
splitOnce _ "" = [""]
splitOnce separator (x:xs)
  | x == separator = ["", xs]
  | otherwise = (x:y):ys
    where (y:ys) = splitOnce separator xs

split' :: Char -> String -> [String]
split' separator s = case splitOnce separator s of
  [a] -> [a]
  [a, b] -> (a:split' separator b)

parseCard :: String -> Card
parseCard s = Card id first second
  where
    parts = (split' ':' s)
    id = read (last (split' ' ' (parts!!0)))
    [first, second] = map (parseInts . trim) (split' '|' (parts!!1))

isWhitespace :: Char -> Bool
isWhitespace = flip elem " \n\t"

trimLeft :: String -> String
trimLeft "" = ""
trimLeft s@(x:xs)
  | isWhitespace x = trimLeft xs
  | otherwise = s

trimRight :: String -> String
trimRight = reverse . trimLeft . reverse

trim :: String -> String
trim = trimLeft . trimRight

parseInts :: String -> [Int]
parseInts = map read . filter (not . null) . (split' ' ')

count :: [Bool] -> Int
count = length . filter id

countWinningNumbers :: Card -> Int
countWinningNumbers (Card _ win actual) = count $ map (flip elem win) actual

evaluateLine :: String -> (Int, [Int])
evaluateLine s = (id, range (id + 1, id + winningNumbers))
  where
    (Card id _ _) = parseCard s
    winningNumbers = countWinningNumbers (parseCard s)

type CardsCache = Map.Map Int [Int]

countCards :: CardsCache -> [Int] -> Int
countCards _ [] = 0
countCards cache cardIds = 1 + (sum $ map sumCopies cardIds)
  where
    sumCopies id = case Map.lookup id cache of
      Just copies -> countCards cache copies
      Nothing -> 1

main :: IO ()
main = do
  contents <- readFile "data.txt"
  let cardIds = map ((\(Card id _ _) -> id) . parseCard) $ lines contents
  let cardsCache = Map.fromList $ filter (not . null . snd) $ map evaluateLine $ lines contents
  let totalNumCards = map (cache ) cardIds
  print $ totalNumCards - 1
