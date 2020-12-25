module Day25 where

import Data.List (find)
import Data.Maybe (fromJust)
import Data.Semigroup (stimes)
import GHC.List (iterate, scanl')
import GHC.TypeLits

newtype Zp = Zp Integer deriving (Show, Eq)

instance Semigroup Zp where
  (Zp x) <> (Zp y) = Zp $ x * y `mod` 20201227

enc :: Zp -> Zp -> Zp
enc (Zp key) x = stimes key x

-- enc key x = foldr (\_ acc -> acc * x `mod` 20201227) 1 [1 .. key]

-- | Solves enc 7 i == pk
computeSK :: Integer -> Integer
computeSK pk =
  fst . fromJust . find (\(_, pk') -> pk' == pk) $
    scanl' (\(_, acc) i -> (i, acc * 7 `mod` 20201227)) (0, 1) [1 ..]

part1 :: Integer -> Integer -> Zp
part1 cardPK doorPK = enc (Zp sk) (Zp doorPK)
  where
    sk = computeSK cardPK

-- solve = do
--   -- fileRaw <- T.readFile $ "inputs/day20_test.txt"
--   file <- readInput 24
--   let input = T.splitOn "\n" file
--   print $ part1 input
--   print $ part2 input

cardPKSample = 5764801 :: Integer

doorPKSample = 17807724 :: Integer

cardPK = 14222596 :: Integer

doorPK = 4057428 :: Integer
