module Day08 where

import Text.ParserCombinators.Parsec
import Parser
import qualified Data.Map as Map

day08 :: IO ()
day08 = do
   input <- readFile "inputs/Day08.txt"
   let documents = parseDocuments input
   print $ "Part 1: " ++ show (part1 documents)
   print $ "Part 2: " ++ show (part2 documents)

part1 :: Documents (Char, Char, Char) -> Int
part1 = steps ('A', 'A', 'A')

part2 :: a -> Integer
part2 _ = 0

-- Types

data Documents a = Documents { instuctions :: [Map.Map a (a, a) -> a -> a]
                             , network :: Map.Map a (a, a) }

type Location = (Char, Char, Char)

l :: Ord a => Map.Map a (a, a) -> a -> a
l m s = fst $ m Map.! s

r :: Ord a => Map.Map a (a, a) -> a -> a
r m s = snd $ m Map.! s

-- Soloutions

steps :: Location -> Documents Location -> Int
steps ('Z', 'Z', 'Z') _ = 0
steps s Documents{instuctions=(i:is), network=n} = 1 + steps (i n s) (Documents is n)
steps _ Documents{instuctions=[]} = error "Empty instuctions"

-- Parser

parseDocuments :: String -> Documents Location
parseDocuments = parseText documentsParser

documentsParser :: Parser (Documents Location)
documentsParser = (Documents . cycle <$> many instuctionParser) <*> networkParser

instuctionParser :: Ord a => Parser (Map.Map a (a, a) -> a -> a)
instuctionParser =
    (l <$ symbol "L") <|>
    (r <$ symbol "R")

networkParser :: Parser (Map.Map Location (Location, Location))
networkParser = Map.fromList <$> many nodeParser

nodeParser :: Parser (Location, (Location, Location))
nodeParser = (,) <$> locationParser <*> (symbol "=" *> parens ((,) <$> locationParser <*> (comma *> locationParser)))

locationParser :: Parser Location
locationParser = lexeme $ (,,) <$> anyChar <*> anyChar <*> anyChar
