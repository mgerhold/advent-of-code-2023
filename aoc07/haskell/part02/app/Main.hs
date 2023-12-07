module Main where

import Data.List (sort)
import Data.List (group)
import Data.Char (digitToInt)

data Hand = Hand String Int deriving Show

determineRating :: String -> Int
determineRating s = case groups of
    [5] -> 7  -- five of a kind
    [4, 1] -> 6 -- four of a kind
    [3, 2] -> 5 -- full house
    [3, 1, 1] -> 4 -- three of a kind
    [2, 2, 1] -> 3 -- two pair
    [2, 1, 1, 1] -> 2 -- one pair
    [1, 1, 1, 1, 1] -> 1 -- high card
  where
    groups = reverse $ sort $ map length (group $ sort s)

highestPossibleRating :: String -> Int
highestPossibleRating s
    | numJokers >= 4 || (numJokers == 3 && numGroups == 1) = 7
    | numJokers == 3 && numGroups == 2 = 6
    | numJokers == 0 = determineRating s
    | otherwise = maximum $ map determineRating (permutations s)
  where
    numGroups = length $ group $ sort $ filter (/= 'J') s
    numJokers = length $ filter (== 'J') s

permutations :: String -> [String]
permutations "" = [""]
permutations (x:xs)
    | x == 'J' = concat $ map prependToLists "AKQT98765432"
    | otherwise = prependToLists x
  where
    prependToLists c = map (c:) (permutations xs)

cardValue :: Char -> Int
cardValue c = case c of
  'A' -> 14
  'K' -> 13
  'Q' -> 12
  'J' -> 1
  'T' -> 10
  _ -> digitToInt c

enumerate :: [a] -> [(Int, a)]
enumerate [] = []
enumerate (x:xs) = ((0, x):map (\(i, e) -> (i + 1, e)) (enumerate xs))

instance Eq Hand where
  (Hand s1 _) == (Hand s2 _) = rating1 == rating2
    where
      rating1 = highestPossibleRating s1
      rating2 = highestPossibleRating s2

instance Ord Hand where
  (Hand s1 _) `compare` (Hand s2 _)
      | rating1 > rating2 = GT
      | rating1 < rating2 = LT
      | otherwise = valueCompare s1 s2
    where
      rating1 = highestPossibleRating s1
      rating2 = highestPossibleRating s2
      valueCompare :: String -> String -> Ordering
      valueCompare [] [] = EQ
      valueCompare (x:xs) (y:ys)
          | first /= second = first `compare` second
          | otherwise = valueCompare xs ys
        where
          first = cardValue x
          second = cardValue y

main :: IO ()
main = do
  contents <- readFile "data.txt"
  let a = sum $ map calculateScore $ enumerate $ sort $ map createHand $ lines contents
        where
          createHand :: String -> Hand
          createHand s = (\[cards, bid] -> Hand cards $ read bid) . words $ s
          calculateScore :: (Int, Hand) -> Int
          calculateScore (index, (Hand _ bid)) = (index + 1) * bid
  print a
