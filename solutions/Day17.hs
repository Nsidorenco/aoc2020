{-# LANGUAGE OverloadedStrings #-}

module Day17 where

import Data.List (sort)
import qualified Data.Map as M
import Data.Maybe (catMaybes)
import qualified Data.Text as T
import FileHelper

type GridMap = M.Map (Int, Int, Int) Char

sampleInput =
  [ ".#.",
    "..#",
    "###"
  ]

input =
  [ "...#...#",
    "..##.#.#",
    "###..#..",
    "........",
    "...##.#.",
    ".#.####.",
    "...####.",
    "..##...#"
  ]

updateCube :: [Char] -> Char -> Char
updateCube neighbours cube =
  case cube of
    '#' -> if n == 2 || n == 3 then '#' else '.'
    '.' -> if n == 3 then '#' else '.'
  where
    n = length $ filter (== '#') neighbours

initialState :: [String] -> GridMap
initialState input =
  foldr f map (zip input [0 ..])
  where
    map = M.empty
    f (row, i) map = foldr (\(col, j) acc -> M.insert (i, j, 0) col acc) map (zip row [0 ..])

findBorders :: GridMap -> ((Int, Int), (Int, Int), (Int, Int))
findBorders =
  M.foldrWithKey maxTuple ((0, 0), (0, 0), (0, 0))
  where
    maxTuple (x, y, z) v acc@((imin, imax), (jmin, jmax), (kmin, kmax)) =
      if v == '#' then ((newVal x imin, newVal' x imax), (newVal y jmin, newVal' y jmax), (newVal z kmin, newVal' z kmax)) else acc
    newVal x i = if x - 1 < i then x - 1 else i
    newVal' x i = if i < x + 1 then x + 1 else i

findNeighbours :: GridMap -> (Int, Int, Int) -> [Char]
findNeighbours map (x, y, z) =
  [M.findWithDefault '.' (x + i, y + j, z + k) map | i <- [-1, 0, 1], j <- [-1, 0, 1], k <- [-1, 0, 1], (i, j, k) /= (0, 0, 0)]

simulateOnce :: GridMap -> GridMap
simulateOnce map = foldr f map ids
  where
    ((xmin, xmax), (ymin, ymax), (zmin, zmax)) = findBorders map
    ids = [(i, j, k) | i <- [xmin .. xmax], j <- [ymin .. ymax], k <- [zmin .. zmax]]
    f point map' =
      M.insert point (updateCube (findNeighbours map point) (M.findWithDefault '.' point map)) map'

simulate :: GridMap -> Int -> GridMap
simulate map 0 = map
simulate map n = simulate (simulateOnce map) (n - 1)

countActive :: GridMap -> Int
countActive = M.foldr (\x acc -> if x == '#' then acc + 1 else acc) 0

part1 :: [String] -> Int
part1 input = countActive $ simulate (initialState input) 6

type GridMap4 = M.Map (Int, Int, Int, Int) Char

initialState4 :: [String] -> GridMap4
initialState4 input =
  foldr f map (zip input [0 ..])
  where
    map = M.empty
    f (row, i) map = foldr (\(col, j) acc -> M.insert (i, j, 0, 0) col acc) map (zip row [0 ..])

findBorders4 :: GridMap4 -> ((Int, Int), (Int, Int), (Int, Int), (Int, Int))
findBorders4 =
  M.foldrWithKey maxTuple ((0, 0), (0, 0), (0, 0), (0, 0))
  where
    maxTuple (x, y, z, q) v acc@((imin, imax), (jmin, jmax), (kmin, kmax), (hmin, hmax)) =
      if v == '#' then ((newVal x imin, newVal' x imax), (newVal y jmin, newVal' y jmax), (newVal z kmin, newVal' z kmax), (newVal q hmin, newVal' q hmax)) else acc
    newVal x i = if x - 1 < i then x - 1 else i
    newVal' x i = if i < x + 1 then x + 1 else i

findNeighbours4 :: GridMap4 -> (Int, Int, Int, Int) -> [Char]
findNeighbours4 map (x, y, z, q) =
  [ M.findWithDefault '.' (x + i, y + j, z + k, q + h) map
    | i <- [-1, 0, 1],
      j <- [-1, 0, 1],
      k <- [-1, 0, 1],
      h <- [-1, 0, 1],
      (i, j, k, h) /= (0, 0, 0, 0)
  ]

simulateOnce4 :: GridMap4 -> GridMap4
simulateOnce4 map = foldr f map ids
  where
    ((xmin, xmax), (ymin, ymax), (zmin, zmax), (qmin, qmax)) = findBorders4 map
    ids = [(i, j, k, q) | i <- [xmin .. xmax], j <- [ymin .. ymax], k <- [zmin .. zmax], q <- [qmin .. qmax]]
    f point map' =
      M.insert point (updateCube (findNeighbours4 map point) (M.findWithDefault '.' point map)) map'

simulate4 :: GridMap4 -> Int -> GridMap4
simulate4 map 0 = map
simulate4 map n = simulate4 (simulateOnce4 map) (n - 1)

countActive4 :: GridMap4 -> Int
countActive4 = M.foldr (\x acc -> if x == '#' then acc + 1 else acc) 0

part2 :: [String] -> Int
part2 input = countActive4 $ simulate4 (initialState4 input) 6

-- solve :: IO ()
-- solve = do
--   file <- readInput 16
--   let input = P.parseOnly parseInput file
--   either print (print . part1) input
--   either print (print . part2) input
