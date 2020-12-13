{-# LANGUAGE OverloadedStrings #-}

module Day10 where

import qualified Data.MemoCombinators as Memo
import qualified Data.Text as T
import FileHelper
import GHC.OldList (sort)

sampleInput =
  [ 16,
    10,
    15,
    5,
    1,
    11,
    7,
    19,
    6,
    12,
    4
  ] ::
    [Int]

sampleInput2 =
  [ 28,
    33,
    18,
    42,
    31,
    14,
    46,
    20,
    48,
    47,
    24,
    23,
    49,
    45,
    19,
    38,
    39,
    11,
    1,
    32,
    25,
    35,
    8,
    17,
    7,
    9,
    4,
    2,
    34,
    10,
    3
  ] ::
    [Int]

part1Aux :: [Int] -> Int -> Int -> Int -> (Int, Int)
part1Aux [] ones threes _ = (ones, threes + 1)
part1Aux (x : xs) ones threes prev =
  case x - prev of
    1 -> part1Aux xs (ones + 1) threes x
    2 -> part1Aux xs ones threes x
    3 -> part1Aux xs ones (threes + 1) x
    n -> error "not sorted"

part2Aux :: Int -> [Int] -> Int -> Int
part2Aux = Memo.memo3 Memo.integral (Memo.list Memo.integral) Memo.integral part2Aux'
  where
    part2Aux' max [] prev = if max - prev <= 3 then 1 else 0
    part2Aux' max (x : xs) prev =
      case x - prev of
        1 -> part2Aux max xs x + part2Aux max xs prev
        2 -> part2Aux max xs x + part2Aux max xs prev
        3 -> part2Aux max xs x + part2Aux max xs prev
        n -> 0

part1 :: [Int] -> Int
part1 input = let (a, b) = part1Aux (sort input) 0 0 0 in a * b

part2 :: [Int] -> Int
part2 input =
  let sorted = sort input
   in part2Aux (last sorted + 3) sorted 0

solve :: IO ()
solve = do
  file <- readInput 10
  let input = map (read . T.unpack) $ T.splitOn "\n" file
  print $ part1 input
  print $ part2 input

-- print $ part2 input 25
