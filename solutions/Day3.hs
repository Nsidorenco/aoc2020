{-# LANGUAGE OverloadedStrings #-}

module Day2 where

import qualified Data.Text as T
import FileHelper

sampleInput =
  [ "..##.......",
    "#...#...#..",
    ".#....#..#.",
    "..#.#...#.#",
    ".#...##..#.",
    "..#.##.....",
    ".#.#.#....#",
    ".#........#",
    "#.##...#...",
    "#...##....#",
    ".#..#...#.#"
  ]

hitTree :: [String] -> (Int, Int) -> Bool
hitTree input (right, down) = (input !! down) !! (right `mod` length (head input)) == '#'

hitTree' :: [String] -> Int -> Int -> Bool
hitTree' input right down = (input !! down) !! (right `mod` length (head input)) == '#'

part1 :: Int -> Int -> [String] -> Int
part1 right down input =
  length . filter (hitTree input) $ zip [right, right + right ..] [down, down + down .. n]
  where
    n = length input - 1

part2 :: [String] -> Int
part2 input =
  product [part1 right down input | (right, down) <- [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]]

solve :: IO ()
solve = do
  file <- readInput 3
  let input = T.unpack <$> T.splitOn "\n" file
  print $ part1 3 1 input
  print $ part2 input
