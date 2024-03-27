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
day04Part2 cards = sum $ Map.elems $ foldl processCard Map.empty cards

-- Types

data Card = Card { cardNumber :: Integer
                 , winningNumbers :: Set.Set Integer
                 , yourNumbers :: [Integer] } deriving(Show)

-- Soloutions

processCard :: Data.Map Integer Integer -> Card -> Data.Map Integer Integer
processCard m c = addScratchcards (cardNumber c) (toInteger $ yourWinningNumbersCount c) (Map.insertWith (+) (cardNumber c) 1 m)

addScratchcards :: Integer -> Integer -> Data.Map Integer Integer -> Data.Map Integer Integer
addScratchcards _ 0 m = m
addScratchcards i p m = addScratchcards i (p-1) (Map.insertWith (+) (i + p) (fromJust $ Map.lookup i m) m)

points :: Card -> Integer
points card = case yourWinningNumbersCount card of
    0 -> 0
    x -> 2^(x-1)

yourWinningNumbersCount :: Card -> Int
yourWinningNumbersCount Card{winningNumbers=ws,yourNumbers=ys} = length $ filter (`Set.member` ws) ys

-- Parser

parseCard :: String -> Card
parseCard line = case parse cardParser "" line of
    Right result -> result
    e -> error $ show e

cardParser :: Parser Card
cardParser = Card <$> (symbol "Card" *> natural) <*> (colon *> (Set.fromList <$> many natural)) <*> (pipe *> many natural)
