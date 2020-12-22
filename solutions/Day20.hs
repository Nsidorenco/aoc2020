{-# LANGUAGE OverloadedStrings #-}

module Day19 where

import Control.Monad (replicateM)
import Control.Monad.List (join)
import Data.Attoparsec.Text (Parser, char, decimal, endOfLine, isEndOfLine, parseOnly, sepBy, string, takeTill)
import Data.Either (rights)
import qualified Data.IntMap as M
import qualified Data.IntSet as S
import Data.List (findIndices, foldl1', sort, subsequences, transpose)
import Data.Maybe (fromJust, isNothing)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Debug.Trace
import FileHelper

parseInput :: Parser (Int, [T.Text])
parseInput = do
  string "Tile "
  n <- decimal
  char ':'
  endOfLine
  maze <- sepBy (takeTill isEndOfLine) endOfLine

  return (n, maze)

flipX :: [T.Text] -> [T.Text]
flipX = fmap T.reverse

flipY :: [T.Text] -> [T.Text]
flipY = reverse

validRight (l, r) = all (\(l, r) -> T.last l == T.head r) $ zip l r

validUp :: ([T.Text], [T.Text]) -> Bool
validUp (u, d) = last u == head d

combinations :: M.IntMap [T.Text] -> Int -> Maybe ([[T.Text]], [Int])
combinations map target = placeGrid [] S.empty
  where
    keys = S.fromList $ M.keys map
    placeGrid :: [[T.Text]] -> S.IntSet -> Maybe ([[T.Text]], [Int])
    placeGrid sol taken
      | S.empty == (keys S.\\ taken) = Just ([], [])
      | otherwise = S.foldr f Nothing (keys S.\\ taken)
      where
        f key acc =
          foldr
            ( \f acc ->
                ( if isNothing acc && valid sol (f (map M.! key))
                    then case placeGrid (sol ++ [f (map M.! key)]) (S.insert key taken) of
                      Just (sol, ks) -> Just (f (map M.! key) : sol, key : ks)
                      _ -> Nothing
                    else acc
                )
            )
            acc
            (foldr (.) id <$> subsequences [T.transpose, flipX, flipY])
        up = length sol - target
        haveUp = 0 <= up
        left = length sol - 1
        haveLeft = 0 <= left && length sol `mod` target /= 0
        valid sol grid
          | haveUp && haveLeft = validRight (sol !! left, grid) && validUp (sol !! up, grid)
          | haveUp = validUp (sol !! up, grid)
          | haveLeft = validRight (sol !! left, grid)
          | otherwise = True

referenceID :: Int -> [Int] -> Int
referenceID n order =
  order !! 0 * order !! (n -1) * order !! (k - n) * order !! (k - 1)
  where
    k = length order

solution =
  [ [1951, 2311, 3079],
    [2729, 1427, 2473],
    [2971, 1489, 1171]
  ]

part1 :: [T.Text] -> Int
part1 input =
  fromJust $ referenceID n . snd <$> combinations map n
  where
    n = floor . sqrt . fromIntegral $ length input
    map = M.map (filter (/= "")) $ M.fromList $ rights $ parseOnly parseInput <$> input

seaMonsterFilter :: [T.Text] -> Bool
seaMonsterFilter (top : mid : bot : _) =
  top `T.index` 18 == '#'
    && ( all ((== '#') . fst) $
           filter (\(_, i) -> i `elem` [0, 5, 6, 11, 12, 17, 18, 19]) (zip (T.unpack mid) [0 ..])
       )
    && ( all ((== '#') . fst) $
           filter (\(_, i) -> i `elem` [1, 4, 7, 10, 13, 16]) (zip (T.unpack bot) [0 ..])
       )

constructMap :: Int -> [[T.Text]] -> [T.Text]
constructMap n grids =
  concat $ collapseRows <$> grid
  where
    collapseRow :: [T.Text] -> [T.Text] -> [T.Text]
    collapseRow acc row = zipWith T.append acc row
    collapseRows :: [[T.Text]] -> [T.Text]
    collapseRows row = foldl1 collapseRow row
    grid = [[stripped !! (i + (n * shift)) | i <- [0 .. n - 1]] | shift <- [0 .. length grids `quot` n - 1]]
    stripped = map strip grids
    strip grid = T.tail . T.init <$> (init . tail) grid

findMonster :: [T.Text] -> Int
findMonster map =
  foldr f 0 (foldr (.) id <$> subsequences [T.transpose, flipX, flipY])
  where
    f :: ([T.Text] -> [T.Text]) -> Int -> Int
    f g acc =
      let n = (length $ filter (\(i, j) -> seaMonsterFilter $ reduceMap g i j) (indices (g map)))
       in if n /= 0 then n else acc
    indices grid =
      let k = T.length (head grid)
       in [(i, j) | i <- [0 .. k - 20], j <- [0 .. (length grid - 3)]]
    reduceMap g i j = T.take 20 . T.drop i <$> take 3 (drop j (g map))

roughness :: [T.Text] -> Int -> Int
roughness grid monsters = good - bad
  where
    bad = monsters * 15
    good = T.length (T.filter (== '#') $ T.concat grid)

part2 :: [T.Text] -> Int
part2 input =
  roughness map monsters
  where
    n = floor . sqrt . fromIntegral $ length input
    grid = M.map (filter (/= "")) $ M.fromList $ rights $ parseOnly parseInput <$> input
    map = constructMap n (fst . fromJust $ combinations grid n)
    monsters = findMonster map

solve = do
  -- fileRaw <- T.readFile $ "inputs/day20_test.txt"
  file <- readInput 20
  let input = T.splitOn "\n\n" file
  print $ part1 input
  print $ part2 input
