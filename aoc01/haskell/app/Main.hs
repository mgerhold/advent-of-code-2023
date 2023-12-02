module Main where

import Data.Maybe (fromJust)

isDigit :: Char -> Bool
isDigit c = c >= '0' && c <= '9'

findFirstDigit :: String -> Maybe Char
findFirstDigit [] = Nothing
findFirstDigit (x:xs) | isDigit x = Just x
findFirstDigit (x:xs) = findFirstDigit xs

appendElement :: [a] -> a -> [a]
appendElement [] a = [a]
appendElement (x:xs) a = (x:appendElement xs a)

reverse' :: String -> String
reverse' [] = []
reverse' (x:xs) = appendElement (reverse' xs) x

findLastDigit :: String -> Maybe Char
findLastDigit = findFirstDigit . reverse'

getFirstAndLastDigits :: String -> String
getFirstAndLastDigits s = [fromJust (findFirstDigit s), fromJust (findLastDigit s)]

sum' :: Num a => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

main :: IO ()
main = do
  contents <- readFile "data.txt"
  print $ sum' $ map (read . getFirstAndLastDigits) (lines contents)
