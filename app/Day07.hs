module Day07 where

import Text.ParserCombinators.Parsec ( (<|>), many, Parser )
import Parser ( parseText, symbol, natural )
import Data.List (sort, group, partition)

day07 :: IO ()
day07 = do
   input <- readFile "inputs/Day07.txt"
   let hands1 = parseHands1 input
   print $ "Part 1: " ++ show (part1 hands1)
   let hands2 = parseHands2 input
   print $ "Part 2: " ++ show (part2 hands2)

part1 :: [Hand1] -> Integer
part1 = totalWinnings1

part2 :: [Hand2] -> Integer
part2 = totalWinnings2

-- Types

data Hand1 = Hand1 { cards1 :: (Card1, Card1, Card1, Card1, Card1)
                 , bid1 :: Integer } deriving (Eq, Show)

data Hand2 = Hand2 { cards2 :: (Card2, Card2, Card2, Card2, Card2)
                 , bid2 :: Integer } deriving (Eq, Show)

handType1 :: Hand1 -> HandType
handType1 Hand1{cards1=(a, b, c, d, e)} = case sort $ map length $ group $ sort [a, b, c, d, e] of
    [1, 1, 1, 1, 1] -> HighCard
    [1, 1, 1, 2] -> OnePair
    [1, 2, 2] -> TwoPair
    [1, 1, 3] -> ThreeOfAKind
    [2, 3] -> FullHouse
    [1, 4] -> FourOfAKind
    [5] -> FiveOfAKind
    cs -> error $ show cs

handType2 :: Hand2 -> HandType
handType2 Hand2{cards2=(a, b, c, d, e)} =
    let
        (js, cs) = partition (Joker2==) [a, b, c, d, e]
        jokers = length js
        baseHandCounts = sort $ map length $ group $ sort cs
        handCounts = if jokers == 5 then [5] else init baseHandCounts ++ [last baseHandCounts + jokers]
    in
        case handCounts of
            [1, 1, 1, 1, 1] -> HighCard
            [1, 1, 1, 2] -> OnePair
            [1, 2, 2] -> TwoPair
            [1, 1, 3] -> ThreeOfAKind
            [2, 3] -> FullHouse
            [1, 4] -> FourOfAKind
            [5] -> FiveOfAKind
            _ -> error $ show handCounts

data Card1 = Two1 | Three1 | Four1 | Five1 | Six1 | Seven1 | Eight1 | Nine1 | Ten1 | Jack1 | Queen1 | King1 | Ace1 deriving (Enum, Eq, Ord, Show)

data Card2 = Joker2 | Two2 | Three2 | Four2 | Five2 | Six2 | Seven2 | Eight2 | Nine2 | Ten2 | Queen2 | King2 | Ace2 deriving (Enum, Eq, Ord, Show)

data HandType = HighCard | OnePair | TwoPair | ThreeOfAKind | FullHouse | FourOfAKind | FiveOfAKind deriving (Enum, Eq, Ord, Show)

instance Ord Hand1 where
    compare a b = compare (handType1 a, cards1 a) (handType1 b, cards1 b)

instance Ord Hand2 where
    compare a b = compare (handType2 a, cards2 a) (handType2 b, cards2 b)

-- Soloutions

totalWinnings1 :: [Hand1] -> Integer
totalWinnings1 hs = sum $ zipWith ((*) . bid1) (sort hs) [1..]

totalWinnings2 :: [Hand2] -> Integer
totalWinnings2 hs = sum $ zipWith ((*) . bid2) (sort hs) [1..]

-- Parser

parseHands1 :: String -> [Hand1]
parseHands1 = parseText (many hand1Parser)

hand1Parser :: Parser Hand1
hand1Parser = Hand1 <$> ((,,,,) <$> card1Parser <*> card1Parser <*> card1Parser <*> card1Parser <*> card1Parser) <*> natural

card1Parser :: Parser Card1
card1Parser =
    (Two1 <$ symbol "2") <|>
    (Three1 <$ symbol "3") <|>
    (Four1 <$ symbol "4") <|>
    (Five1 <$ symbol "5") <|>
    (Six1 <$ symbol "6") <|>
    (Seven1 <$ symbol "7") <|>
    (Eight1 <$ symbol "8") <|>
    (Nine1 <$ symbol "9") <|>
    (Ten1 <$ symbol "T") <|>
    (Jack1 <$ symbol "J") <|>
    (Queen1 <$ symbol "Q") <|>
    (King1 <$ symbol "K") <|>
    (Ace1 <$ symbol "A")

parseHands2 :: String -> [Hand2]
parseHands2 = parseText (many hand2Parser)

hand2Parser :: Parser Hand2
hand2Parser = Hand2 <$> ((,,,,) <$> card2Parser <*> card2Parser <*> card2Parser <*> card2Parser <*> card2Parser) <*> natural

card2Parser :: Parser Card2
card2Parser =
    (Joker2 <$ symbol "J") <|>
    (Two2 <$ symbol "2") <|>
    (Three2 <$ symbol "3") <|>
    (Four2 <$ symbol "4") <|>
    (Five2 <$ symbol "5") <|>
    (Six2 <$ symbol "6") <|>
    (Seven2 <$ symbol "7") <|>
    (Eight2 <$ symbol "8") <|>
    (Nine2 <$ symbol "9") <|>
    (Ten2 <$ symbol "T") <|>
    (Queen2 <$ symbol "Q") <|>
    (King2 <$ symbol "K") <|>
    (Ace2 <$ symbol "A")
