module Day06 where

import Text.ParserCombinators.Parsec
import Parser
import Data.Foldable.Extra (productOn')
import Data.Char (isSpace)
day06 :: IO ()
day06 = do
   input <- readFile "inputs/Day06.txt"
   let races1 = parseRaces input
   print $ "Part 1: " ++ show (solve races1)
   let races2 = parseRaces $ filter (not . isSpace) input
   print $ "Part 2: " ++ show (solve races2)

solve :: [Race] -> Integer
solve = productOn' numberOfWaysYouCanBeatTheRecord

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
        b = sqrt ((t ** 2) - (4 * d)) / (2 :: Double)
        x = a + b
        y = a - b
        low = ceiling $ min x y
        high = floor $ max x y
        -- Make sure these aren't draws
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
