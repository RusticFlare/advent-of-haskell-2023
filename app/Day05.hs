module Day05 where

import Text.ParserCombinators.Parsec
import Parser
import Data.Range

day05 :: IO ()
day05 = do
   input <- readFile "inputs/Day05.txt"
   let almanac1 = parseAlmanac1 input
   print $ "Part 1: " ++ show (solve almanac1)
   let almanac2 = parseAlmanac2 input
   print $ "Part 2: " ++ show (solve almanac2)

solve :: Almanac -> Integer
solve almanac = head $ fromRanges $ locations almanac

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
sourceRanges AlmanacRange{sourceStart=s, rangeLength=l} = [s +=> l]

shift :: AlmanacRange -> Integer
shift AlmanacRange{destinationStart=d, sourceStart=s} = d - s

-- Soloutions

locations :: Almanac -> [Range Integer]
locations Almanac{seeds=ss, maps=ms} = foldl applyMap ss ms

applyMap :: [Range Integer] -> AlmanacMap -> [Range Integer]
applyMap sources AlmanacMap{ranges=rs} = applyRanges rs sources

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

parseAlmanac1 :: String -> Almanac
parseAlmanac1 = parseText almanac1Parser

parseAlmanac2 :: String -> Almanac
parseAlmanac2 = parseText almanac2Parser

almanac1Parser :: Parser Almanac
almanac1Parser = Almanac <$> (symbol "seeds:" *> many (SingletonRange <$> natural)) <*> many almanacMapParser

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
