{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Day19 where

import Control.Monad (replicateM)
import Control.Monad.List (join)
import Data.Attoparsec.Text (Parser, char, decimal, endOfLine, isEndOfLine, parseOnly, sepBy, skipSpace, string, takeTill)
import Data.Either (rights)
import Data.List (find, findIndices, foldl', foldl1', intercalate, sort, sortBy, subsequences, transpose)
import qualified Data.Map as M
import Data.Maybe (fromJust, isNothing)
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Debug.Trace
import FileHelper

sampleInput =
  [ "mxmxvkd kfcds sqjhc nhms (contains dairy, fish)",
    "trh fvjkl sbzzf mxmxvkd (contains dairy)",
    "sqjhc fvjkl (contains soy)",
    "sqjhc mxmxvkd sbzzf (contains fish)"
  ] ::
    [T.Text]

parseFood :: Parser ([String], [String])
parseFood = do
  ingredients <- takeTill (== '(')
  string "(contains"

  allergens <- sepBy parseIngredients (char ',')

  return (words $ T.unpack ingredients, T.unpack <$> allergens)
  where
    parseIngredients = do
      skipSpace
      takeTill (\x -> x == ',' || x == ')')

parseFoods = rights . (parseOnly parseFood <$>)

findRules :: [([String], [String])] -> [(String, [String])]
findRules input = go $ filter (\(_, as) -> not $ null as) $ sortBy (\(_, e1) (_, e2) -> compare (length e1) (length e2)) input
  where
    go [] = []
    go ((ingredients, allergens) : xs) =
      map (,ingredients) allergens ++ go xs

combineRules :: [(String, [String])] -> [(String, S.Set String)]
combineRules = go S.empty
  where
    go :: S.Set String -> [(String, [String])] -> [(String, S.Set String)]
    go _ [] = []
    go taken ((allergen, ingredients) : xs) =
      case S.member allergen taken of
        True -> go taken xs
        _ ->
          ( allergen,
            foldr
              ( \(a, ingredients') acc ->
                  if a == allergen then S.intersection acc (S.fromList ingredients') else acc
              )
              (S.fromList ingredients)
              xs
          ) :
          go (S.insert allergen taken) xs

-- Needs to do backtracking
solveRules :: [(String, S.Set String)] -> M.Map String String
solveRules rules = fromJust $ go S.empty $ sortBy (\(_, e1) (_, e2) -> compare (S.size e1) (S.size e2)) rules
  where
    go :: S.Set String -> [(String, S.Set String)] -> Maybe (M.Map String String)
    go _ [] = Just M.empty
    go taken ((allergen, o) : xs) =
      let options = S.difference o taken
       in foldl'
            ( \acc option ->
                case go (S.insert option taken) xs of
                  Just m -> Just (M.insert allergen option m)
                  Nothing -> acc
            )
            Nothing
            options

-- M.insert allergen option (go (S.insert option taken) xs)

countFoods :: M.Map String String -> [([String], [String])] -> Int
countFoods map foods = length $ filter (not . knowFoods) totalFood
  where
    knowFoods :: String -> Bool
    knowFoods a = a `elem` M.elems map
    totalFood = concatMap fst foods

part1 :: [T.Text] -> Int
part1 input =
  let parsed = parseFoods input
   in countFoods (solveRules $ combineRules $ findRules parsed) parsed

part2 :: [T.Text] -> String
part2 input =
  let parsed = parseFoods input
      sol = solveRules $ combineRules $ findRules parsed
      sorted = sortBy (\(k1, _) (k2, _) -> compare k1 k2) $ zip (M.keys sol) (M.elems sol)
   in intercalate "," $ map snd sorted

solve = do
  -- fileRaw <- T.readFile $ "inputs/day20_test.txt"
  file <- readInput 21
  let input = T.splitOn "\n" file
  print $ part1 input
  print $ part2 input
