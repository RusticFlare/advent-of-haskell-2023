module Day05 where

import Text.ParserCombinators.Parsec
import Parser
import Control.Monad (msum)
import Data.Maybe (fromMaybe)
import Data.Foldable (Foldable(foldl'))

day05 :: IO ()
day05 = do
   input <- readFile "inputs/Day05.txt"
   let almanac = parseAlmanac input
   print $ "Part 1: " ++ show (day05Part1 almanac)
   print $ "Part 2: " ++ show (day05Part2 almanac)

day05Part1 :: Almanac -> Integer
day05Part1 almanac = minimum $ locations almanac

day05Part2 :: Almanac -> Integer
day05Part2 _ = 0

-- Types

data Almanac = Almanac { seeds :: [Integer]
                       , maps :: [AlmanacMap] } deriving (Show)

data AlmanacMap = AlmanacMap { sourceCategory :: String
                             , destinationCategory :: String
                             , ranges :: [AlmanacRange] } deriving (Show)

data AlmanacRange = AlmanacRange { destinationStart :: Integer
                                 , sourceStart :: Integer
                                 , rangeLength :: Integer } deriving (Show)

-- Soloutions

locations :: Almanac -> [Integer]
locations almanac = map (seedLocation almanac) (seeds almanac)

seedLocation :: Almanac -> Integer -> Integer
seedLocation almanac seed = foldl' applyMap seed $ maps almanac

applyMap :: Integer ->  AlmanacMap -> Integer
applyMap input almanacMap = fromMaybe input $ msum appliedRanges
    where appliedRanges = map (applyRange input) $ ranges almanacMap

applyRange :: Integer -> AlmanacRange -> Maybe Integer
applyRange input range
    | input >= s && input < s + l = Just (d - s + input)
    | otherwise = Nothing
    where s = sourceStart range
          l = rangeLength range
          d = destinationStart range

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
