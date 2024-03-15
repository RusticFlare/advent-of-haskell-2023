module Day02 where

import qualified Data.Map as Map
import Text.ParserCombinators.Parsec
import Parser

day02 :: IO ()
day02 = do
   input <- readFile "inputs/Day02.txt"
   print $ day02Part1 input
   print $ day02Part2 input

data Colour = Red | Green | Blue deriving (Enum, Eq, Ord, Show)

newtype Round = Round { balls :: Map.Map Colour Integer } deriving (Show)

data Game = Game { gameNumber :: Integer
                , rounds :: [Round] } deriving (Show)

day02Part1 :: String -> Integer
day02Part1 input = sum $ map (score . parseGame) $ lines input

day02Part2 :: String -> Integer
day02Part2 input = sum $ map (power . parseGame) $ lines input

parseGame :: String -> Game
parseGame line = case parse gameParser "" line of
    Right result ->  result
    e -> error $ show e

gameParser :: Parser Game
gameParser = Game <$> (symbol "Game" *> natural) <*> (colon *> semiSep roundParser)

roundParser :: Parser Round
roundParser = Round . Map.fromList <$> commaSep ballCountParser

ballCountParser :: Parser (Colour, Integer)
ballCountParser = reverseTuple <$> natural <*> colourParser
    where reverseTuple a b = (b, a)

colourParser :: Parser Colour
colourParser = (Blue <$ string "blue") <|> (Green <$ string "green") <|> (Red <$ string "red")

score :: Game -> Integer
score game
    | gameIsPossible game = gameNumber game
    | otherwise = 0

gameIsPossible :: Game -> Bool
gameIsPossible game = all roundIsPossible $ rounds game

roundIsPossible :: Round -> Bool
roundIsPossible r = colourIsPossible Red 12 r && colourIsPossible Green 13 r && colourIsPossible Blue 14 r

colourIsPossible :: Colour -> Integer -> Round -> Bool
colourIsPossible colour maxCount r = Map.findWithDefault 0 colour (balls r) <= maxCount

power :: Game -> Integer
power game = product $ Map.elems $ foldl (Map.unionWith max) Map.empty $ map balls $ rounds game
