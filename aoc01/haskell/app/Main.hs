module Main where

import Data.Maybe (fromJust)
import Data.Maybe (fromMaybe)

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

startsWith :: String -> String -> Bool
startsWith _ [] = True
startsWith [] (x:xs) = False
startsWith (x:xs) (y:ys) | x == y = startsWith xs ys
startsWith (x:xs) (y:ys) = False

startsWithOneOf :: String -> [String] -> Maybe Int
startsWithOneOf s [] = Nothing
startsWithOneOf s (x:xs) | startsWith s x = Just 0
startsWithOneOf s (x:xs) = fmap (+1) (startsWithOneOf s xs)

findFirstDigitOrNumberWord :: String -> Maybe Int
findFirstDigitOrNumberWord [] = Nothing
findFirstDigitOrNumberWord (x:xs) | isDigit x = Just (read [x])
findFirstDigitOrNumberWord (x:xs) = maybe
  (findFirstDigitOrNumberWord xs) -- default value if there is "Nothing"
  (Just . (+1)) -- function to apply to the value if it is "Just ..."
  (startsWithOneOf (x:xs) ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]) -- value to inspect

findLastDigitOrNumberWord :: String -> Maybe Int
findLastDigitOrNumberWord [] = Nothing
findLastDigitOrNumberWord (x:xs) = maybe (findFirstDigitOrNumberWord (x:xs)) Just (findLastDigitOrNumberWord xs)

getFirstAndLastDigitsOrNumberWords :: String -> Int
getFirstAndLastDigitsOrNumberWords s = 10 * fromJust (findFirstDigitOrNumberWord s) + fromJust (findLastDigitOrNumberWord s)

main :: IO ()
main = do
  contents <- readFile "data.txt"
  print $ sum' $ map (read . getFirstAndLastDigits) (lines contents)
  print $ sum' $ map getFirstAndLastDigitsOrNumberWords (lines contents)
