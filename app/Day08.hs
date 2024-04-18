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

part1 :: Documents -> Int
part1 = steps "AAA"

part2 :: a -> Integer
part2 _ = 0

-- Types

data Documents = Documents { instuctions :: [Map.Map String (String, String) -> String -> String]
                           , network :: Map.Map String (String, String) }

l :: Map.Map String (String, String) -> String -> String
l m s = fst $ m Map.! s

r :: Map.Map String (String, String) -> String -> String
r m s = snd $ m Map.! s

-- Soloutions

steps :: String -> Documents -> Int
steps "ZZZ" _ = 0
steps s Documents{instuctions=(i:is), network=n} = 1 + steps (i n s) (Documents is n)
steps _ Documents{instuctions=[]} = error "Empty instuctions"

-- Parser

parseDocuments :: String -> Documents
parseDocuments = parseText documentsParser

documentsParser :: Parser Documents
documentsParser = (Documents . cycle <$> many instuctionParser) <*> networkParser

instuctionParser :: Parser (Map.Map String (String, String) -> String -> String)
instuctionParser =
    (l <$ symbol "L") <|>
    (r <$ symbol "R")

networkParser :: Parser (Map.Map String (String, String))
networkParser = Map.fromList <$> many nodeParser

nodeParser :: Parser (String, (String, String))
nodeParser = (,) <$> identifier <*> (symbol "=" *> parens ((,) <$> identifier <*> (comma *> identifier)))
