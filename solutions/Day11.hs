{-# LANGUAGE OverloadedStrings #-}

module Day10 where

import qualified Data.Text as T
import FileHelper

sampleInput =
  [ "L.LL.LL.LL",
    "LLLLLLL.LL",
    "L.L.L..L..",
    "LLLL.LL.LL",
    "L.LL.LL.LL",
    "L.LLLLL.LL",
    "..L.L.....",
    "LLLLLLLLLL",
    "L.LLLLLL.L",
    "L.LLLLL.LL"
  ]

type ADJ = [String] -> Int -> Int -> String

type Update = [String] -> ADJ -> Int -> Int -> Char -> Char

adjacentOccupied :: ADJ
adjacentOccupied input i j =
  filter
    (== '#')
    [ (input !! a) !! b | a <- [i -1, i, i + 1], b <- [j -1, j, j + 1], 0 <= a && a < length input && 0 <= b && b < length (head input) && (a, b) /= (i, j)
    ]

changeSeat1 :: Update
changeSeat1 input f i j c =
  case c of
    'L' -> if null occupied then '#' else 'L'
    '#' -> if 4 <= length occupied then 'L' else '#'
    _ -> c
  where
    occupied = f input i j

stepState :: [String] -> ADJ -> Update -> [String]
stepState input adj update =
  zipWith
    ( \row i ->
        zipWith (flip $ update input adj i) row [0 ..]
    )
    input
    [0 ..]

fix :: Eq a => (a -> a) -> a -> a
fix f x =
  let x' = f x
   in if x' == x then x else fix f x'

part1 :: [String] -> Int
part1 input = foldr (\row acc -> acc + length (filter (== '#') row)) 0 (fix (\x -> stepState x adjacentOccupied changeSeat1) input)

firstInDirection :: [String] -> (Int, Int) -> Int -> Int -> Char
firstInDirection input dir@(di, dj) i j
  | i < 0 || length input <= i || j < 0 || length (head input) <= j = '.'
  | otherwise =
    case (input !! i) !! j of
      'L' -> 'L'
      '#' -> '#'
      _ -> firstInDirection input dir (i + di) (j + dj)

anyOccupied :: ADJ
anyOccupied input i j =
  filter
    (== '#')
    [firstInDirection input (di, dj) (i + di) (j + dj) | di <- [-1, 0, 1], dj <- [-1, 0, 1], (di, dj) /= (0, 0)]

changeSeat2 :: Update
changeSeat2 input adj i j c =
  case c of
    'L' -> if null occupied then '#' else 'L'
    '#' -> if 5 <= length occupied then 'L' else '#'
    _ -> c
  where
    occupied = adj input i j

part2 :: [String] -> Int
part2 input = foldr (\row acc -> acc + length (filter (== '#') row)) 0 (fix (\x -> stepState x anyOccupied changeSeat2) input)

solve :: IO ()
solve = do
  file <- readInput 11
  let input = map T.unpack $ T.splitOn "\n" file
  print input
  print $ part1 input
  print $ part2 input
