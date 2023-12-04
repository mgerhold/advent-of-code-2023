module Main where

import Data.Maybe (fromJust)
import Data.Typeable (typeOf)

data ParserResult = UnitResult ()
  | CharResult Char
  | StringResult String
  | IntResult Int
  | ColorResult Color
  | SubDrawResult SubDraw
  | DrawResult Draw
  | DrawListResult DrawList
  | GameResult Game deriving Show

data Color = Red | Green | Blue deriving Show

data SubDraw = SubDraw Int Color deriving Show

data Draw = Draw [SubDraw] deriving Show

data DrawList = DrawList [Draw] deriving Show

data Game = Game Int DrawList deriving Show

parseAnyChar :: String -> Maybe (ParserResult, String)
parseAnyChar [] = Nothing
parseAnyChar (x:xs) = Just (CharResult x, xs)

isOneOf :: Char -> [Char] -> Bool
isOneOf _ [] = False
isOneOf c (x:xs) | c == x = True
isOneOf c (x:xs) = isOneOf c xs

isWhitespace :: Char -> Bool
isWhitespace c | isOneOf c " \t\n" = True
isWhitespace _ = False

isDigit :: Char -> Bool
isDigit c = (c >= '0') && (c <= '9')

discardWhitespace :: String -> Maybe (ParserResult, String)
discardWhitespace [] = Just (UnitResult (), "")
discardWhitespace (x:xs) | isWhitespace x = discardWhitespace xs
discardWhitespace s = Just (UnitResult (), s)

sequence' :: [(String -> Maybe(ParserResult, String))] -> String -> Maybe ([ParserResult], String)
sequence' [] s = Just ([], s)
sequence' (x:xs) s = case x s of
  Just (result, remaining) -> case sequence' xs remaining of
    Just (sub_result, sub_remaining) -> Just ((result:sub_result), sub_remaining)
    Nothing -> Nothing
  Nothing -> Nothing

parseChar :: Char -> String -> Maybe (ParserResult, String)
parseChar c s = case parseAnyChar s of
  Just (CharResult result, remaining) | result == c -> Just (CharResult result, remaining)
  Just (CharResult actual, _) -> Nothing
  Nothing -> Nothing

parseString :: String -> String -> Maybe (ParserResult, String)
parseString [] s = Just (StringResult "", s)
parseString (x:xs) s = case parseChar x s of
  Just (CharResult result, remaining) -> case parseString xs remaining of
    Just (StringResult sub_result, sub_remaining) -> Just (StringResult (result:sub_result), sub_remaining)
    Nothing -> Nothing
  Nothing -> Nothing

parseWhile :: (Char -> Bool) -> String -> Maybe (ParserResult, String)
parseWhile _ [] = Just (StringResult "", "")
parseWhile predicate (x:xs) | predicate x = case parseWhile predicate xs of
  Just (StringResult result, remaining) -> Just (StringResult (x:result), remaining)
parseWhile _ s = Just (StringResult "", s)

parseInt :: String -> Maybe (ParserResult, String)
parseInt s = case parseWhile isDigit s of
  Just (StringResult result, remaining) | result /= "" -> Just (IntResult (read result), remaining)
  Just (StringResult result, remaining) -> Nothing

repeat' :: (String -> Maybe (ParserResult, String)) -> String -> Maybe [ParserResult]
repeat' parser s = case parser s of
  Just (result, remaining) -> case repeat' parser remaining of
    Just sub_result -> Just (result:sub_result)
    Nothing -> Nothing
  Nothing | s == "" -> Just []
  Nothing -> Nothing

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' predicate (x:xs) | predicate x = (x:filter' predicate xs)
filter' predicate (x:xs) = filter' predicate xs

isUnitResult :: ParserResult -> Bool
isUnitResult (UnitResult ()) = True
isUnitResult _ = False

parseAnyOf :: [String -> Maybe (ParserResult, String)] -> String -> Maybe (ParserResult, String)
parseAnyOf [] _ = Nothing
parseAnyOf (x:xs) s = case x s of
  Just result -> Just result
  Nothing -> parseAnyOf xs s

parseColor :: String -> Maybe (ParserResult, String)
parseColor s@(x:xs) = case parseAnyOf [parseString "red", parseString "green", parseString "blue"] s of
  Just (StringResult "red", remaining) -> Just (ColorResult Red, remaining)
  Just (StringResult "green", remaining) -> Just (ColorResult Green, remaining)
  Just (StringResult "blue", remaining) -> Just (ColorResult Blue, remaining)
  Nothing -> Nothing
parseColor [] = Nothing

parseSubDraw :: String -> Maybe (ParserResult, String)
parseSubDraw s = case list of
    Just ([IntResult amount, UnitResult (), ColorResult color], remaining) -> Just (SubDrawResult $ SubDraw amount color, remaining)
    Nothing -> Nothing
  where list = sequence' [parseInt, discardWhitespace, parseColor] s

discardWhitespaceAndParse :: (String -> Maybe (ParserResult, String)) -> String -> Maybe (ParserResult, String)
discardWhitespaceAndParse parser s = case sequence' [discardWhitespace, parser] s of
  Just (results, remaining) -> Just (results!!1, remaining)
  Nothing -> Nothing

parseList :: Char -> (String -> Maybe (ParserResult, String)) -> String -> Maybe ([ParserResult], String)
parseList _ _ [] = Just ([], "")
parseList separator parser s@(x:xs) = case discardWhitespaceAndParse parser s of
  Just (result, remaining) -> case discardWhitespaceAndParse (parseChar separator) remaining of
    Just (_, sub_remaining) -> case parseList separator (discardWhitespaceAndParse parser) sub_remaining of
      Just (sub_list, sub_sub_remaining) -> Just ((result:sub_list), sub_sub_remaining)
      Nothing -> Nothing
    Nothing -> Just ([result], remaining)
  Nothing -> Just ([], "")

asSubDraws :: [ParserResult] -> [SubDraw]
asSubDraws [] = []
asSubDraws (SubDrawResult x:xs) = (x:asSubDraws xs)

asDraws :: [ParserResult] -> [Draw]
asDraws [] = []
asDraws (DrawResult x:xs) = (x:asDraws xs)

parseDraw :: String -> Maybe (ParserResult, String)
parseDraw s = case parseList ',' parseSubDraw s of
  Just (sub_draws, remaining) -> Just (DrawResult (Draw (asSubDraws sub_draws)), remaining)
  Nothing -> Nothing

parseDrawList :: String -> Maybe (ParserResult, String)
parseDrawList [] = Nothing
parseDrawList s = case parseList ';' (discardWhitespaceAndParse parseDraw) s of
  Just (draws, remaining) -> Just (DrawListResult (DrawList (asDraws draws)), remaining)
  Nothing -> Nothing

parseGame :: String -> Maybe (ParserResult, String)
parseGame [] = Nothing
parseGame s = case list of
    Just ([StringResult _, IntResult id, CharResult _, DrawListResult drawlist], remaining) -> Just (GameResult (Game id drawlist), remaining)
    _ -> Nothing
  where list = fmap (\(x, y) -> (filter' (not . isUnitResult) x, y)) (sequence' [
                      parseString "Game",
                      discardWhitespaceAndParse parseInt,
                      parseChar ':',
                      parseDrawList
                    ] s)

parseGames :: String -> Maybe([ParserResult])
parseGames = repeat' (discardWhitespaceAndParse parseGame)

isSubDrawPossible :: SubDraw -> Bool
isSubDrawPossible (SubDraw amount Red) | amount <= 12 = True
isSubDrawPossible (SubDraw amount Green) | amount <= 13 = True
isSubDrawPossible (SubDraw amount Blue) | amount <= 14 = True
isSubDrawPossible _ = False

all' :: (a -> Bool) -> [a] -> Bool
all' _ [] = True
all' predicate (x:xs) | predicate x = all' predicate xs
all' _ _ = False

isDrawPossible :: Draw -> Bool
isDrawPossible (Draw list) = all' isSubDrawPossible list

isGamePossible :: Game -> Bool
isGamePossible (Game _ (DrawList list)) = all' isDrawPossible list

asGames :: [ParserResult] -> [Game]
asGames [] = []
asGames (GameResult x:xs) = (x:asGames xs)

fold' :: Num n => n -> (n -> n -> n) -> [n] -> n
fold' initialValue _ [] = initialValue
fold' initialValue operation (x:xs) = operation x (fold' initialValue operation xs)

sum' :: Num n => [n] -> n
sum' list = fold' 0 (\x y -> x + y) list

product' :: Num n => [n] -> n
product' list = fold' 1 (\x y -> x * y) list

getPossibleGames :: String -> Maybe [Game]
getPossibleGames s = fmap ((filter' isGamePossible) . asGames) (parseGames s)

getGameId :: Game -> Int
getGameId (Game id _) = id

getMinimumDraw :: Game -> Draw
getMinimumDraw (Game _ (DrawList [draw])) = draw

main :: IO ()
main = do
  contents <- readFile "data.txt"
  print $ fromJust $ fmap (sum . map getGameId) (getPossibleGames contents) -- part 1
