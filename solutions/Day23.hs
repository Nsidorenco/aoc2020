module Day23 where

import Data.Foldable (Foldable (foldl'), toList)
import Data.Maybe (fromJust)
import Data.Sequence (Seq, deleteAt, elemIndexL, iterateN, (!?), (><))
import qualified Data.Sequence as S
import Debug.Trace (traceShow)

sampleInput = S.fromList [3, 8, 9, 1, 2, 5, 4, 6, 7] :: Seq Int

cyclicLookup :: Seq Int -> Int -> Int
cyclicLookup seq n = fromJust $ seq !? (n `mod` length seq)

step :: Seq Int -> Int -> (Int, Seq Int)
step st i = (k, st')
  where
    n = length st
    cur = i `mod` n
    taken = S.fromList $ [cyclicLookup st (cur + i) | i <- [1, 2, 3]]
    k = fromJust $ elemIndexL (cyclicLookup st (cur + 4)) st'
    lowerI = lower (cyclicLookup st cur)
    deleted = foldr deleteAt st ((S.sort . S.fromList) [(cur + i) `mod` n | i <- [1 .. 3]])
    st' =
      S.take (lowerI + 1) deleted
        >< taken
        >< S.take (n - (lowerI + 4)) (S.drop (lowerI + 1) deleted)
    lower :: Int -> Int
    lower e =
      if e < minimum deleted
        then fromJust $ elemIndexL (maximum deleted) deleted
        else maybe (lower (e - 1)) id (elemIndexL (e - 1) deleted)

run :: Seq Int -> Int -> (Int, Seq Int)
run st n =
  foldr (\_ (i, st') -> step st' i) (0, st) [0 .. (n - 1)]

part1 :: Seq Int -> Seq Int
part1 st = snd $ run st 100

part2 :: Seq Int -> Int
part2 st = cyclicLookup st' (i + 1) * cyclicLookup st' (i + 2)
  where
    n = maximum st
    i = fromJust $ elemIndexL 1 st'
    (_, st') = run (st >< (S.fromList [n .. 1000000])) 10000000

-- solve = do
--   print $ part1 p1 p2
--   print $ part2 p1 p2

puzzle = [3, 6, 4, 2, 9, 7, 5, 8, 1] :: [Int]
