module Day08 where

import Text.ParserCombinators.Parsec ( Parser, (<|>), many, anyChar )
import Parser ( comma, lexeme, parens, parseText, symbol )
import qualified Data.Map as Map

day08 :: IO ()
day08 = do
   input <- readFile "inputs/Day08.txt"
   let documents = parseDocuments input
   print $ "Part 1: " ++ show (part1 documents)
   print $ "Part 2: " ++ show (part2 documents)

part1 :: Documents -> Int
part1  d = steps d (('Z', 'Z', 'Z')==) ('A', 'A', 'A')

part2 :: Documents -> Int
part2 d = foldr (lcm . steps d (locationEndsWith 'Z')) 1 (filter (locationEndsWith 'A') $ locations d)

-- Types

data Documents = Documents { instuctions :: [Map.Map Location (Location, Location) -> Location -> Location]
                           , network :: Map.Map Location (Location, Location) }

turns :: Documents -> [Location -> Location]
turns Documents{instuctions=is, network=n} = [ i n | i <- is ]

locations :: Documents -> [Location]
locations Documents{network=n} = Map.keys n

type Location = (Char, Char, Char)

l :: Ord a => Map.Map a (a, a) -> a -> a
l m s = fst $ m Map.! s

r :: Ord a => Map.Map a (a, a) -> a -> a
r m s = snd $ m Map.! s

-- Soloutions

iterate' :: [a -> a] -> a -> [a]
iterate' [] _ =  []
iterate' (f:fs) x =  x : iterate' fs (f x)

path :: Documents -> Location -> [Location]
path docs = iterate' $ turns docs

steps :: Documents -> (Location -> Bool) -> Location -> Int
steps documents endCondition start = length $ takeWhile (not . endCondition) (path documents start)

locationEndsWith :: Char -> Location -> Bool
locationEndsWith c (_, _, z) = c == z

-- Parser

parseDocuments :: String -> Documents
parseDocuments = parseText documentsParser

documentsParser :: Parser Documents
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
