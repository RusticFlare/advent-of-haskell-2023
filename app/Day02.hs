module Day02 where

import qualified Data.Map as Map
import Text.ParserCombinators.Parsec
import Parser
import Data.Function((&))
import Data.Foldable.Extra (sumOn')

day02 :: IO ()
day02 = do
   input <- readFile "inputs/Day02.txt"
   let games = map parseGame $ lines input
   print $ "Part 1: " ++ show (day02Part1 games)
   print $ "Part 2: " ++ show (day02Part2 games)

data Colour = Red | Green | Blue deriving (Enum, Eq, Ord, Show)

newtype Round = Round { balls :: Map.Map Colour Integer } deriving (Show)

data Game = Game { gameNumber :: Integer
                , rounds :: [Round] } deriving (Show)

day02Part1 :: [Game] -> Integer
day02Part1 = sumOn' score

day02Part2 :: [Game] -> Integer
day02Part2 = sumOn' power

parseGame :: String -> Game
parseGame line = case parse gameParser "" line of
    Right result -> result
    e -> error $ show e

gameParser :: Parser Game
gameParser = Game <$> (symbol "Game" *> natural) <*> (colon *> semiSep roundParser)

roundParser :: Parser Round
roundParser = Round . Map.fromList <$> commaSep ballCountParser

ballCountParser :: Parser (Colour, Integer)
ballCountParser = flip (,) <$> natural <*> colourParser

colourParser :: Parser Colour
colourParser = (Blue <$ string "blue") <|> (Green <$ string "green") <|> (Red <$ string "red")

score :: Game -> Integer
score game
    | gameIsPossible game = game & gameNumber
    | otherwise = 0

gameIsPossible :: Game -> Bool
gameIsPossible game = all roundIsPossible $ rounds game

roundIsPossible :: Round -> Bool
roundIsPossible r = all (uncurry (colourIsPossible r)) [(Red, 12), (Green, 13), (Blue, 14)]

colourIsPossible :: Round -> Colour -> Integer -> Bool
colourIsPossible r colour maxCount = Map.findWithDefault 0 colour (balls r) <= maxCount

power :: Game -> Integer
power game = product $ Map.elems $ foldl (Map.unionWith max) Map.empty $ map balls $ rounds game
