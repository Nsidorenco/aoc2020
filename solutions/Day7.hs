{-# LANGUAGE OverloadedStrings #-}

module Day7 where

import Data.Attoparsec.Text
import Data.Char (isDigit)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import FileHelper

sampleInput' =
  "light red bags contain 1 bright white bag, 2 muted yellow bags." :: Text

parseString :: Parser [(Text, [(Int, Text)])]
parseString = do
  start <- manyTill anyChar " contain"
  many' space
  rest <-
    commaSep
      ( do
          many' space
          n <- many' digit
          many' space
          bag <- takeTill (inClass ",.\n")
          return (read n, bag)
      )
  "."

  return [(T.pack start, rest)]
  where
    -- return [(T.pack start, (1, [T.pack start]))]

    commaSep p = p `sepBy` char ','

sampleInput =
  [ "light red bags contain 1 bright white bag, 2 muted yellow bags.",
    "dark orange bags contain 3 bright white bags, 4 muted yellow bags.",
    "bright white bags contain 1 shiny gold bag.",
    "muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.",
    "shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.",
    "dark olive bags contain 3 faded blue bags, 4 dotted black bags.",
    "vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.",
    "faded blue bags contain no other bags.",
    "dotted black bags contain no other bags."
  ] ::
    [Text]

sampleNesting =
  [ "shiny gold bags contain 2 dark red bags.",
    "dark red bags contain 2 dark orange bags.",
    "dark orange bags contain 2 dark yellow bags.",
    "dark yellow bags contain 2 dark green bags.",
    "dark green bags contain 2 dark blue bags.",
    "dark blue bags contain 2 dark violet bags.",
    "dark violet bags contain no other bags. "
  ] ::
    [Text]

target = "shiny gold bag" :: Text

parseInput :: [Text] -> [[Text]]
parseInput input =
  let stripped1 = map (T.replace " contain" ",") input
   in let stripped2 = map (T.replace "bags" "bag") stripped1
       in let stripped3 = map (T.replace "." "") stripped2
           in map (map T.strip . T.splitOn ", " . T.filter (not . isDigit)) stripped3

parseInput2 :: [Text] -> [[Text]]
parseInput2 input =
  let stripped1 = map (T.replace (T.pack "contain no other bags") "") input
   in let stripped2 = map (T.replace " contain" ",") stripped1
       in let stripped3 = map (T.replace "bags" "bag") stripped2
           in let stripped4 = map (T.replace "." "") stripped3
               in map (map T.strip . T.splitOn ", ") stripped4

parseToMap :: [[Text]] -> Map Text [Text]
parseToMap = foldr (\txts -> M.insertWith (++) (head txts) (tail txts)) M.empty

searchColor :: Map Text [Text] -> Text -> Text -> Bool
searchColor rules target current =
  let res = M.lookup current rules
   in case res of
        (Just xs) ->
          foundTarget || foldr (\cur acc -> acc || searchColor rules target cur) False xs
          where
            foundTarget = foldr (\cur acc -> acc || cur == target) False xs
        Nothing -> False

part1 :: [Text] -> Text -> Int
part1 input target =
  length $ M.filterWithKey (\k _ -> searchColor rules target k) rules
  where
    rules = parseToMap $ parseInput input

searchNesting :: Map Text [Text] -> Text -> Int
searchNesting rules current =
  let res = M.lookup current rules
   in case res of
        (Just xs) ->
          foldr (\cur acc -> acc + (read [T.head cur] * searchNesting rules (T.drop 2 cur))) 1 xs
        Nothing -> 1

part2 :: [Text] -> Text -> Int
part2 input =
  flip (-) 1 . searchNesting rules
  where
    rules = parseToMap $ parseInput2 input

solve :: IO ()
solve = do
  file <- readInput 7
  let input = T.splitOn "\n" file
  print $ part2 input "shiny gold bag"
