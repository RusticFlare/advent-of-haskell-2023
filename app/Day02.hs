module Day02 where

import qualified Data.Map as Map
import Text.Parsec

day02 :: IO ()
day02 = do
   input <- readFile "inputs/Day02.txt"
   print $ day02Part1 input
   print $ day02Part2 input

data Colour = Red | Green | Blue deriving (Enum, Eq, Ord, Show)

newtype Round = Round { balls :: Map.Map Colour Int } deriving (Show)

data Game = Game { gameNumber :: Int
                , rounds :: [Round] } deriving (Show)

day02Part1 :: String -> Int
day02Part1 input = sum $ map (score . parseGame) $ lines input

day02Part2 :: String -> Int
day02Part2 input = sum $ map (power . parseGame) $ lines input

parseGame :: String -> Game
parseGame line = case parse gameParser "" line of
    Right result ->  result
    e -> error $ show e

gameParser :: Parsec String st Game
gameParser = toGame <$> (string "Game " *> intParser) <*> (string ": " *> roundParser `sepBy` string "; ")
    where toGame n rs = Game { gameNumber = n, rounds = rs }

roundParser :: Parsec String st Round
roundParser = toRound <$> ballCountParser `sepBy` string ", "
    where toRound ballList = Round { balls = Map.fromList ballList }

ballCountParser :: Parsec String st (Colour, Int)
ballCountParser = reverseTuple <$> intParser <*> (char ' ' *> colourParser)
    where reverseTuple a b = (b, a)

colourParser :: Parsec String st Colour
colourParser = try (Blue <$ string "blue") <|> try (Green <$ string "green") <|> (Red <$ string "red")

intParser :: Parsec String st Int
intParser = read <$> many1 digit

score :: Game -> Int
score game
    | gameIsPossible game = gameNumber game
    | otherwise = 0

gameIsPossible :: Game -> Bool
gameIsPossible game = all roundIsPossible $ rounds game

roundIsPossible :: Round -> Bool
roundIsPossible r = colourIsPossible Red 12 r && colourIsPossible Green 13 r && colourIsPossible Blue 14 r

colourIsPossible :: Colour -> Int -> Round -> Bool
colourIsPossible colour maxCount r = Map.findWithDefault 0 colour (balls r) <= maxCount

power :: Game -> Int
power game = product $ Map.elems $ foldl (Map.unionWith max) Map.empty $ map balls $ rounds game
