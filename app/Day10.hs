module Day10 where

import Text.ParserCombinators.Parsec ( (<|>), Parser, many )
import Parser ( parseText, symbol )
import Data.List
import Data.Maybe (isJust, fromJust, catMaybes, mapMaybe)

day10 :: IO ()
day10 = do
   input <- readFile "inputs/Day10.txt"
   let tiles = map parseTiles $ lines input
   print $ "Part 1: " ++ show (part1 tiles)
   print $ "Part 2: " ++ show (part2 tiles)

part1 :: [[Tile]] -> Int
part1 tss =
    let
        (l, r) = startDirections tss
        paths = zip (follow tss l) (follow tss r)
        paths' = takeWhile (\((_, s'), (_, s'')) -> s' /= s'') paths
    in
        2 + length paths'

part2 :: a -> Integer
part2 _ = 0

-- Types

data Direction = North | East | South | West deriving (Enum, Eq, Ord, Show, Bounded)

opposite :: Direction -> Direction
opposite North = South
opposite South = North
opposite East = West
opposite West = East

data Tile = Pipe { d1 :: Direction, d2 :: Direction } | Ground | Start deriving (Eq, Show)

travel :: Tile -> Direction -> Maybe Direction
travel Pipe{d1=d',d2=d''} d
    | d' == d = Just d''
    | d'' == d = Just d'
    | otherwise = Nothing
travel Ground _ = Nothing
travel Start _ = error "Can't enter the start"

(!!!) :: [[Tile]] -> (Int, Int) -> Tile
(!!!) tss (r, c) = (tss !! r) !! c

-- Soloutions

startLocation :: [[Tile]] -> (Int, Int)
startLocation tss = (fromJust (findIndex isJust cs), head (catMaybes cs))
    where cs = map (elemIndex Start) tss

startDirections :: [[Tile]] -> ((Direction, (Int, Int)), (Direction, (Int, Int)))
startDirections tss =
    let
        start = startLocation tss
        xs = map (proceed start) [North .. West]
        (l:r:[]) = mapMaybe (maybeNext tss) xs
    in
        (l, r)

follow :: [[Tile]] -> (Direction, (Int, Int)) -> [(Direction, (Int, Int))]
follow tss = iterate (next tss)

next :: [[Tile]] -> (Direction, (Int, Int)) -> (Direction, (Int, Int))
next tss (d, s) = fromJust $ maybeNext tss (d, s)

maybeNext :: [[Tile]] -> (Direction, (Int, Int)) -> Maybe (Direction, (Int, Int))
maybeNext tss (d, s) = do
        d' <- travel (tss !!! s) d
        return $ proceed s d'

proceed :: (Int, Int) -> Direction -> (Direction, (Int, Int))
proceed s d = (opposite d, move d s)

move :: Direction -> (Int, Int) -> (Int, Int)
move North (r, c) = (r - 1, c)
move South (r, c) = (r + 1, c)
move East  (r, c) = (r, c + 1)
move West  (r, c) = (r, c - 1)

-- Parser

parseTiles :: String -> [Tile]
parseTiles = parseText (many tileParser)

tileParser :: Parser Tile
tileParser =
    (Pipe North South <$ symbol "|") <|>
    (Pipe East West <$ symbol "-") <|>
    (Pipe North East <$ symbol "L") <|>
    (Pipe North West <$ symbol "J") <|>
    (Pipe South West <$ symbol "7") <|>
    (Pipe South East <$ symbol "F") <|>
    (Ground <$ symbol ".") <|>
    (Start <$ symbol "S")
