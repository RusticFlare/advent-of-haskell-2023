module Day07 where

import Text.ParserCombinators.Parsec
import Parser
import Data.List (sort, group)

day07 :: IO ()
day07 = do
   input <- readFile "inputs/Day07.txt"
   let hands = parseHands input
   print $ "Part 1: " ++ show (part1 hands)
   print $ "Part 2: " ++ show (part2 hands)

part1 :: [Hand] -> Integer
part1 = totalWinnings

part2 :: [Hand] -> Integer
part2 _ = 0

-- Types

data Hand = Hand { cards :: (Card, Card, Card, Card, Card)
                 , bid :: Integer } deriving (Eq, Show)

handType :: Hand -> HandType
handType Hand{cards=(a, b, c, d, e)} = case sort $ map length $ group $ sort [a, b, c, d, e] of
    [1, 1, 1, 1, 1] -> HighCard
    [1, 1, 1, 2] -> OnePair
    [1, 2, 2] -> TwoPair
    [1, 1, 3] -> ThreeOfAKind
    [2, 3] -> FullHouse
    [1, 4] -> FourOfAKind
    [5] -> FiveOfAKind
    cs -> error $ show cs

data Card = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace deriving (Enum, Eq, Ord, Show)

data HandType = HighCard | OnePair | TwoPair | ThreeOfAKind | FullHouse | FourOfAKind | FiveOfAKind deriving (Enum, Eq, Ord, Show)

instance Ord Hand where
    compare a b = compare (handType a, cards a) (handType b, cards b)

-- Soloutions

totalWinnings :: [Hand] -> Integer
totalWinnings hs = sum $ zipWith (\h i -> bid h * i) (sort hs) [1..]

-- Parser

parseHands :: String -> [Hand]
parseHands = parseText (many handParser)

handParser :: Parser Hand
handParser = Hand <$> ((,,,,) <$> cardParser <*> cardParser <*> cardParser <*> cardParser <*> cardParser) <*> natural

cardParser :: Parser Card
cardParser =
    (Two <$ symbol "2") <|>
    (Three <$ symbol "3") <|>
    (Four <$ symbol "4") <|>
    (Five <$ symbol "5") <|>
    (Six <$ symbol "6") <|>
    (Seven <$ symbol "7") <|>
    (Eight <$ symbol "8") <|>
    (Nine <$ symbol "9") <|>
    (Ten <$ symbol "T") <|>
    (Jack <$ symbol "J") <|>
    (Queen <$ symbol "Q") <|>
    (King <$ symbol "K") <|>
    (Ace <$ symbol "A")
