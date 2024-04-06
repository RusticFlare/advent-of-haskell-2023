module Day05 where

import Text.ParserCombinators.Parsec
import Parser

day05 :: IO ()
day05 = do
   input <- readFile "inputs/Day05.txt"
   let almanac = parseAlmanac input
   print $ "Part 1: " ++ show (day05Part1 almanac)
   print $ "Part 2: " ++ show (day05Part2 almanac)

day05Part1 :: Almanac -> Integer
day05Part1 _ = 0

day05Part2 :: Almanac -> Integer
day05Part2 _ = 0

-- Types

data Almanac = Almanac { seeds :: [Integer]
                       , maps :: [AlmanacMap] } deriving (Show)

data AlmanacMap = AlmanacMap { sourceCategory :: String
                             , destinationCategory :: String
                             , ranges :: [AlmanacRange] } deriving (Show)

data AlmanacRange = AlmanacRange { destinationRangeStart :: Integer
                                 , sourceRangeStart :: Integer
                                 , rangeLength :: Integer } deriving (Show)

-- Soloutions

-- Parser

parseText :: Parser a -> String -> a
parseText parser text = case parse parser "" text of
    Right result -> result
    Left e -> error $ show e

parseAlmanac :: String -> Almanac
parseAlmanac = parseText almanacParser

almanacParser :: Parser Almanac
almanacParser = Almanac <$> (symbol "seeds:" *> many natural) <*> many almanacMapParser

almanacMapParser :: Parser AlmanacMap
almanacMapParser = AlmanacMap <$> identifier <*> (symbol "-to-" *> identifier) <*> (symbol "map:" *> many almanacRangeParser)

almanacRangeParser :: Parser AlmanacRange
almanacRangeParser = AlmanacRange <$> natural <*> natural <*> natural
