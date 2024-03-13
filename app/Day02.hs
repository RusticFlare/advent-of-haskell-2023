module Day02 where

import Data.List.Split (splitOn)
import qualified Data.Map as Map

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
day02Part2 _ = 0

parseGame :: String -> Game
parseGame line =
    let gameText:roundsText:[] = splitOn ": " line
        _:gameNumberText:[] = words gameText
    in Game { gameNumber = read gameNumberText, rounds = map parseRound (splitOn "; " roundsText) }


parseRound :: String -> Round
parseRound text =
    let ballsText = splitOn ", " text
    in Round { balls = Map.fromList (map parseBallCount ballsText) }

parseBallCount :: String -> (Colour, Int)
parseBallCount text =
    let count:colour:[] = words text
    in (parseColour colour, read count)

parseColour :: String -> Colour
parseColour "blue" = Blue
parseColour "green" = Green
parseColour "red" = Red
parseColour colour = error $ colour ++ " is not a valid Colour"

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
