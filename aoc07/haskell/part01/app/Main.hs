module Main where

import Debug.Trace (traceShowId)
import Data.List (foldl')
import Data.List (sort)
import Data.List (group)
import Data.Char (digitToInt)

data Hand = Hand String Int deriving Show

determineRating :: Hand -> Int
determineRating (Hand s _) = case groups of
    [5] -> 7  -- five of a kind
    [4, 1] -> 6 -- four of a kind
    [3, 2] -> 5 -- full house
    [3, 1, 1] -> 4 -- three of a kind
    [2, 2, 1] -> 3 -- two pair
    [2, 1, 1, 1] -> 2 -- one pair
    [1, 1, 1, 1, 1] -> 1 -- high card
  where
    groups = reverse $ sort $ map length (group $ sort s)

cardValue :: Char -> Int
cardValue 'A' = 14
cardValue 'K' = 13
cardValue 'Q' = 12
cardValue 'J' = 11
cardValue 'T' = 10
cardValue c = digitToInt c

enumerate :: [a] -> [(Int, a)]
enumerate [] = []
enumerate (x:xs) = ((0, x):map (\(i, e) -> (i + 1, e)) (enumerate xs))

instance Eq Hand where
  (Hand s1 _) == (Hand s2 _) = s1 == s2

instance Ord Hand where
  hand1@(Hand s1 _) `compare` hand2@(Hand s2 _) =
    if rating1 > rating2 then GT else (
        if rating1 < rating2 then LT else valueCompare s1 s2
      )
    where
      rating1 = determineRating hand1
      rating2 = determineRating hand2
      valueCompare :: String -> String -> Ordering
      valueCompare [] [] = EQ
      valueCompare (x:xs) (y:ys) = if firstValue /= secondValue then firstValue `compare` secondValue else valueCompare xs ys
        where
          firstValue = cardValue x
          secondValue = cardValue y

main :: IO ()
main = do
  contents <- readFile "data.txt"
  let a = sum $ map ((\(rank, (Hand _ bid)) -> rank * bid) . (\(i, e) -> (i + 1, e))) $ enumerate $ sort $ map ((\[a, b] -> (Hand a (read b))) . words) $ lines contents
  print a
