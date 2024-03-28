module Day04 where

import qualified Data.Set as Set
import qualified Data.Map as Map
import Text.ParserCombinators.Parsec
import Parser
import Data.Foldable.Extra (sumOn')
import qualified Data.Map as Data
import Data.Maybe (fromJust)

day04 :: IO ()
day04 = do
   input <- readFile "inputs/Day04.txt"
   let cards = map parseCard $ lines input
   print $ "Part 1: " ++ show (day04Part1 cards)
   print $ "Part 2: " ++ show (day04Part2 cards)

day04Part1 :: [Card] -> Integer
day04Part1 = sumOn' points

day04Part2 :: [Card] -> Integer
day04Part2 cs = sum $ Map.elems $ foldl processCard Map.empty cs

-- Types

data Card = Card { cardNumber :: Integer
                 , winningNumbers :: Set.Set Integer
                 , yourNumbers :: [Integer] } deriving (Show)

-- Soloutions

processCard :: Data.Map Integer Integer -> Card -> Data.Map Integer Integer
processCard m c = addScratchcards n w m'
    where n = cardNumber c
          w = toInteger $ yourWinningNumbersCount c
          m' = Map.insertWith (+) n 1 m

addScratchcards :: Integer -> Integer -> Data.Map Integer Integer -> Data.Map Integer Integer
addScratchcards _ 0 m = m
addScratchcards i p m = addScratchcards i (p-1) m'
    where m' = Map.insertWith (+) (i + p) (fromJust $ Map.lookup i m) m

points :: Card -> Integer
points c = case yourWinningNumbersCount c of
    0 -> 0
    x -> 2^(x-1)

yourWinningNumbersCount :: Card -> Int
yourWinningNumbersCount Card{winningNumbers=ws,yourNumbers=ys} = length $ filter (`Set.member` ws) ys

-- Parser

parseCard :: String -> Card
parseCard line = case parse cardParser "" line of
    Right result -> result
    Left e -> error $ show e

cardParser :: Parser Card
cardParser = Card <$> (symbol "Card" *> natural) <*> (colon *> (Set.fromList <$> many natural)) <*> (pipe *> many natural)
