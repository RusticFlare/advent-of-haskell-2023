module Day06 where

import Text.ParserCombinators.Parsec
import Parser
import Data.Foldable.Extra (productOn')
day06 :: IO ()
day06 = do
   input <- readFile "inputs/Day06.txt"
   let races = parseRaces input
   print $ "Part 1: " ++ show (day06Part1 races)
   print $ "Part 2: " ++ show (day06Part2 races)

day06Part1 :: [Race] -> Integer
day06Part1 = productOn' numberOfWaysYouCanBeatTheRecord

day06Part2 :: a -> Integer
day06Part2 _ = 0

-- Types

data Race = Race { time :: Integer
                 , distance :: Integer } deriving (Show)

-- Soloutions

numberOfWaysYouCanBeatTheRecord :: Race -> Integer
numberOfWaysYouCanBeatTheRecord race =
    let
        t = fromInteger $ time race
        d = fromInteger $ distance race
        isWin' = isWin race
        a = t / 2
        b = sqrt ((t ** 2) - (4 * d)) / 2
        x = a + b
        y = a - b
        low = ceiling $ min x y
        high = floor $ max x y
        low' = if isWin' low then low else low + 1
        high' = if isWin' high then high else high - 1
    in
        high' - low' + 1

isWin :: Race -> Integer -> Bool
isWin Race{time=t, distance=d} h = h * (t - h) > d

-- Parser

parseRaces :: String -> [Race]
parseRaces = parseText racesParser

racesParser :: Parser [Race]
racesParser = zipWith Race <$> (symbol "Time:" *> many natural) <*> (symbol "Distance:" *> many natural)
