{-# LANGUAGE OverloadedStrings #-}

module Day24 where

import Control.Parallel.Strategies
import Data.Attoparsec.Text (Parser, many', parseOnly, string)
import Data.Bool (bool)
import Data.Either (rights)
import Data.List (foldl')
import qualified Data.Map as M
import Data.Map.Strict (Map, alter)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Debug.Trace (traceShow)
import FileHelper
import GHC.Base (Alternative ((<|>)))

sampleInput =
  [ "sesenwnenenewseeswwswswwnenewsewsw",
    "neeenesenwnwwswnenewnwwsewnenwseswesw",
    "seswneswswsenwwnwse",
    "nwnwneseeswswnenewneswwnewseswneseene",
    "swweswneswnenwsewnwneneseenw",
    "eesenwseswswnenwswnwnwsewwnwsene",
    "sewnenenenesenwsewnenwwwse",
    "wenwwweseeeweswwwnwwe",
    "wsweesenenewnwwnwsenewsenwwsesesenwne",
    "neeswseenwwswnwswswnw",
    "nenwswwsewswnenenewsenwsenwnesesenew",
    "enewnwewneswsewnwswenweswnenwsenwsw",
    "sweneswneswneneenwnewenewwneswswnese",
    "swwesenesewenwneswnwwneseswwne",
    "enesenwswwswneneswsenwnewswseenwsese",
    "wnwnesenesenenwwnenwsewesewsesesew",
    "nenewswnwewswnenesenwnesewesw",
    "eneswnwswnwsenenwnwnwwseeswneewsenese",
    "neswnwewnwnwseenwseesewsenwsweewe",
    "wseweeenwnesenwwwswnew"
  ] ::
    [T.Text]

data Direction
  = East
  | SouthEast
  | SouthWest
  | West
  | NorthWest
  | NorthEast
  deriving (Show)

data Colour = White | Black deriving (Show, Eq)

flipColour Black = White
flipColour White = Black

parseDirections :: Parser [Direction]
parseDirections = many' parseDirection
  where
    parseDirection =
      (string "se" >> return SouthEast)
        <|> (string "sw" >> return SouthWest)
        <|> (string "nw" >> return NorthWest)
        <|> (string "ne" >> return NorthEast)
        <|> (string "e" >> return East)
        <|> (string "w" >> return West)

traversePath :: Map (Int, Int, Int) Colour -> [Direction] -> Map (Int, Int, Int) Colour
traversePath map dirs = alter f (go dirs) map
  where
    f Nothing = Just Black
    f (Just c) = Just $ flipColour c
    go [] = (0, 0, 0)
    go (d : ds) =
      case d of
        East -> (x - 1, y + 1, z)
        West -> (x + 1, y - 1, z)
        NorthEast -> (x, y + 1, z - 1)
        NorthWest -> (x + 1, y, z - 1)
        SouthEast -> (x - 1, y, z + 1)
        SouthWest -> (x, y - 1, z + 1)
      where
        (x, y, z) = go ds

-- problem: E SW SE == SW SW SW

part1 :: [T.Text] -> Int
part1 input = M.foldr (\c acc -> if c == Black then acc + 1 else acc) 0 flipped
  where
    flipped = foldr (\path map -> traversePath map path) M.empty paths
    paths = rights $ parseOnly parseDirections <$> input

maxBounds :: Int -> (Int, Int) -> (Int, Int)
maxBounds val (l, h)
  | val < l = (val, h)
  | h < val = (l, val)
  | otherwise = (l, h)

findBounds :: Map (Int, Int, Int) Colour -> ((Int, Int), (Int, Int), (Int, Int))
findBounds map =
  let ((xmin, xmax), (ymin, ymax), (zmin, zmax)) = M.foldrWithKey f ((0, 0), (0, 0), (0, 0)) map
   in ((xmin -1, xmax + 1), (ymin -1, ymax + 1), (zmin -1, zmax + 1))
  where
    f (x, y, z) _ (xbounds, ybounds, zbounds) =
      (maxBounds x xbounds, maxBounds y ybounds, maxBounds z zbounds)

flipTiles :: Map (Int, Int, Int) Colour -> Map (Int, Int, Int) Colour
flipTiles map = foldr (\(k, mv) acc -> maybe acc (\v -> M.insert k v acc) mv) map updates
  where
    updates = f <$> keys `using` parListChunk 1000 rseq
    ((xmin, xmax), (ymin, ymax), (zmin, zmax)) = findBounds map
    keys = [(x, y, z) | x <- [xmin .. xmax], y <- [ymin .. ymax], z <- [zmin .. zmax], x + y + z == 0]
    f key@(x, y, z) =
      let neighbours = [(x, y + 1, z -1), (x + 1, y, z -1), (x + 1, y -1, z), (x, y -1, z + 1), (x -1, y, z + 1), (x -1, y + 1, z)]
          -- val = maybe White id $ M.lookup key map
          val = fromMaybe White $ M.lookup key map
          b =
            foldr
              ( \k b -> case M.lookup k map of
                  Just Black -> b + 1
                  _ -> b
              )
              0
              neighbours
       in case val of
            White -> if b == 2 then (key, Just Black) else (key, Nothing)
            Black -> if b == 0 || 2 < b then (key, Just White) else (key, Nothing)

simulate :: Map (Int, Int, Int) Colour -> Int -> Map (Int, Int, Int) Colour
simulate map n = foldl' (\map' i -> traceShow (i + 1) $ flipTiles map') map [0 .. n -1]

part2 :: [T.Text] -> Int -> Int
part2 input n = M.foldr (\c acc -> bool acc (acc + 1) (c == Black)) 0 flipped
  where
    flipped = simulate (foldr (flip traversePath) M.empty paths) n
    paths = rights $ parseOnly parseDirections <$> input

sampleMap = foldr (flip traversePath) M.empty $ rights $ parseOnly parseDirections <$> sampleInput

solve = do
  -- fileRaw <- T.readFile $ "inputs/day20_test.txt"
  file <- readInput 24
  let input = T.splitOn "\n" file
  print $ part1 input
  print $ part2 input 100
