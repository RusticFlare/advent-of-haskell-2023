module Day05 where

import Text.ParserCombinators.Parsec
import Parser
import Data.Range

day05 :: IO ()
day05 = do
   input <- readFile "inputs/Day05.txt"
   let almanac = parseAlmanac input
   print $ "Part 1: " ++ show (day05Part1 almanac)
   print $ "Part 2: " ++ show (day05Part2 almanac)

day05Part1 :: Almanac -> Integer
day05Part1 almanac = head $ fromRanges $ locations almanac

day05Part2 :: Almanac -> Integer
day05Part2 _ = 0

-- Types

data Almanac = Almanac { seeds :: [Range Integer]
                       , maps :: [AlmanacMap] } deriving (Show)

data AlmanacMap = AlmanacMap { sourceCategory :: String
                             , destinationCategory :: String
                             , ranges :: [AlmanacRange] } deriving (Show)

data AlmanacRange = AlmanacRange { destinationStart :: Integer
                                 , sourceStart :: Integer
                                 , rangeLength :: Integer } deriving (Show)

sourceRanges :: AlmanacRange -> [Range Integer]
sourceRanges almanacRange = [sourceStart almanacRange +=> rangeLength almanacRange]

shift :: AlmanacRange -> Integer
shift almanacRange = destinationStart almanacRange - sourceStart almanacRange

-- Soloutions

locations :: Almanac -> [Range Integer]
locations a = foldl applyMap (seeds a) (maps a)

applyMap :: [Range Integer] -> AlmanacMap -> [Range Integer]
applyMap sources almanacMap = applyRanges (ranges almanacMap) sources

applyRanges :: [AlmanacRange] -> [Range Integer] -> [Range Integer]
applyRanges [] sources = sources
applyRanges (r:rs) sources = union destinations $ applyRanges rs remainder
    where (remainder, destinations) = applyRange r sources

applyRange :: AlmanacRange -> [Range Integer] -> ([Range Integer], [Range Integer])
applyRange almanacRange sources = (remainder, destinations)
    where sr = sourceRanges almanacRange
          inter = intersection sr sources
          remainder = difference sources inter
          s = shift almanacRange
          destinations = shiftRanges s inter

-- Parser

parseText :: Parser a -> String -> a
parseText parser text = case parse parser "" text of
    Right result -> result
    Left e -> error $ show e

parseAlmanac :: String -> Almanac
parseAlmanac = parseText almanacParser

parseAlmanac2 :: String -> Almanac
parseAlmanac2 = parseText almanac2Parser

almanacParser :: Parser Almanac
almanacParser = Almanac <$> (symbol "seeds:" *> many (SingletonRange <$> natural)) <*> many almanacMapParser

almanac2Parser :: Parser Almanac
almanac2Parser = Almanac <$> (symbol "seeds:" *> many ((+=>) <$> natural <*> natural)) <*> many almanacMapParser

almanacMapParser :: Parser AlmanacMap
almanacMapParser = AlmanacMap <$> identifier <*> (symbol "-to-" *> identifier) <*> (symbol "map:" *> many almanacRangeParser)

almanacRangeParser :: Parser AlmanacRange
almanacRangeParser = AlmanacRange <$> natural <*> natural <*> natural

-- Helper

(+=>) :: Num a => a -> a -> Range a
(+=>) s l = s +=* (s + l)

shiftRanges :: Num a => a -> [Range a] -> [Range a]
shiftRanges s = map $ fmap (+s)
