module Day22 where

import Data.Sequence (Seq)
import qualified Data.Sequence as S
import Debug.Trace
import FileHelper

p1Input =
  [ 9,
    2,
    6,
    3,
    1
  ]

p2Input =
  [ 5,
    8,
    4,
    7,
    10
  ]

playGame :: Seq Int -> Seq Int -> [Int]
playGame = undefined

-- solve = do
--   -- fileRaw <- T.readFile $ "inputs/day20_test.txt"
--   file <- readInput 21
--   let input = T.splitOn "\n" file
--   print $ part1 input
--   print $ part2 input
