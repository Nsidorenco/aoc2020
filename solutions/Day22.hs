module Day22 where

import Data.Foldable (Foldable (toList))
import Data.Sequence (Seq ((:<|)))
import qualified Data.Sequence as Se
import Data.Set (Set)
import qualified Data.Set as S
import Debug.Trace
import FileHelper

playGame :: Seq Int -> Seq Int -> [Int]
playGame Se.Empty s = toList s
playGame s Se.Empty = toList s
playGame (x :<| xs) (y :<| ys) =
  if x < y
    then playGame xs (ys Se.|> y Se.|> x)
    else playGame (xs Se.|> x Se.|> y) ys

part1 :: [Int] -> [Int] -> Int
part1 p1 p2 =
  sum $ zipWith (*) (reverse result) [1 ..]
  where
    result = playGame (Se.fromList p1) (Se.fromList p2)

data Player = Player1 | Player2 deriving (Show, Enum, Eq)

playRecur :: Set (Seq Int) -> Seq Int -> Seq Int -> (Player, [Int])
playRecur _ Se.Empty s = (Player2, toList s)
playRecur _ s Se.Empty = (Player1, toList s)
playRecur played p1@(x :<| xs) p2@(y :<| ys)
  | S.member p1 played || S.member p2 played = (Player1, toList p2)
  | x <= length xs && y <= length ys =
    case fst $ playRecur played (Se.take x xs) (Se.take y ys) of
      Player1 -> playRecur played' (xs Se.|> x Se.|> y) ys
      Player2 -> playRecur played' xs (ys Se.|> y Se.|> x)
  | otherwise =
    if x < y
      then playRecur played' xs (ys Se.|> y Se.|> x)
      else playRecur played' (xs Se.|> x Se.|> y) ys
  where
    played' = S.insert p2 $ S.insert p1 played

part2 :: [Int] -> [Int] -> Int
part2 p1 p2 =
  sum $ zipWith (*) (reverse result) [1 ..]
  where
    result = snd $ playRecur S.empty (Se.fromList p1) (Se.fromList p2)

solve = do
  print $ part1 p1 p2
  print $ part2 p1 p2

p1 =
  [ 30,
    42,
    25,
    7,
    29,
    1,
    16,
    50,
    11,
    40,
    4,
    41,
    3,
    12,
    8,
    20,
    32,
    38,
    31,
    2,
    44,
    28,
    33,
    18,
    10
  ] ::
    [Int]

p2 =
  [ 36,
    13,
    46,
    15,
    27,
    45,
    5,
    19,
    39,
    24,
    14,
    9,
    17,
    22,
    37,
    47,
    43,
    21,
    6,
    35,
    23,
    48,
    34,
    26,
    49
  ] ::
    [Int]

p1Sample =
  [ 9,
    2,
    6,
    3,
    1
  ] ::
    [Int]

p2Sample =
  [ 5,
    8,
    4,
    7,
    10
  ] ::
    [Int]
