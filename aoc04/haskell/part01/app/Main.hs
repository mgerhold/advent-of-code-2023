module Main where

-- Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53

import Debug.Trace (traceShowId)

data Card = Card [Int] [Int] deriving Show

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
parseCard s = Card first second
  where [first, second] = map (parseInts . trim) (split' '|' ((split' ':' s)!!1))

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
countWinningNumbers (Card win actual) = count $ map (flip elem win) actual

countPoints :: Int -> Int
countPoints 0 = 0
countPoints p = (2 ^ (p - 1))

evaluateLine :: String -> Int
evaluateLine = countPoints . countWinningNumbers . parseCard

main :: IO ()
main = do
  contents <- readFile "data.txt"
  print $ sum $ map evaluateLine $ lines contents
