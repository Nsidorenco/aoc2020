{-# LANGUAGE OverloadedStrings #-}

module Day19 where

import Control.Applicative (Alternative ((<|>)))
import Control.Monad (forM, forM_, join, liftM)
import Data.Attoparsec.Text
import Data.Either (rights)
import Data.IntMap ((!))
import qualified Data.IntMap as M
import qualified Data.Text as T
import Debug.Trace
import FileHelper

data Rule
  = Const Char
  | Sequence [Rule]
  | Choice Rule Rule
  deriving (Show, Eq)

sampleInput =
  [ "0: 4 1 5",
    "1: 2 3 | 3 2",
    "2: 4 4 | 5 5",
    "3: 4 5 | 5 4",
    "4: \"a\"",
    "5: \"b\""
  ]

sampleMessages =
  [ "ababbb",
    "bababa",
    "abbbab",
    "aaabbb",
    "aaaabbb"
  ]

inputToMap :: [String] -> M.IntMap String
inputToMap = foldr f M.empty
  where
    f :: String -> M.IntMap String -> M.IntMap String
    f x acc =
      let (n, rest) = filter (/= ':') <$> break (== ':') x
       in M.insert (read n) rest acc

parseConst :: Char -> Parser ()
parseConst c = do
  char c
  return ()

parseChoice :: Parser a -> Parser a -> Parser a
parseChoice = (<|>)

parseSequence :: [Parser a] -> Parser ()
parseSequence parsers = forM_ parsers id

constructParser :: Rule -> Parser ()
constructParser (Const c) = parseConst c
constructParser (Choice r1 r2) = parseChoice (constructParser r1) (constructParser r2)
constructParser (Sequence rules) =
  parseSequence (constructParser <$> rules)

constructRule :: M.IntMap String -> M.IntMap Rule
constructRule map = snd $ go 0 (map ! 0)
  where
    go :: Int -> String -> (Rule, M.IntMap Rule)
    go n rule
      | '\"' `elem` rule =
        let c = Const $ head $ filter (\x -> x /= '\"' && x /= ' ') rule
         in (c, M.insert n c M.empty)
      | '|' `elem` rule =
        let (l, r) = filter (/= '|') <$> break (== '|') rule
            (r1, m1) = go n l
            (r2, m2) = go n r
         in (Choice r1 r2, M.insert n (Choice r1 r2) (M.union m1 m2))
      | otherwise =
        let mapped = fmap (\x -> go (read x) (map ! read x)) (words rule)
            (res, maps) =
              foldr
                ( \(s', m') (s, m) ->
                    (s' : s, M.union m' m)
                )
                ([], M.empty)
                mapped
         in (Sequence res, M.insert n (Sequence res) maps)

filterParses :: IResult T.Text () -> Bool
filterParses (Done "" _) = True
filterParses (Partial _) = True
filterParses _ = False

findParses :: Parser () -> [String] -> [()]
findParses parser msgs =
  rights (parseOnly parser . T.pack <$> msgs)

part1 :: [String] -> [String] -> Int
part1 rules msgs = length $ findParses (parser >> endOfInput) msgs
  where
    parser = constructParser $ (constructRule $ inputToMap rules) ! 0

part2 :: [String] -> [String] -> Int
part2 r msgs = length $ findParses (parser >> endOfInput) msgs
  where
    rules = constructRule $ inputToMap r
    p42 = constructParser (rules ! 42)
    p31 = constructParser (rules ! 31)
    parser = do
      r41 <- many1 p42
      r32 <- many1 p31
      if length r41 > length r32 then return () else fail ""

complicatedRules =
  [ "42: 9 14 | 10 1",
    "9: 14 27 | 1 26",
    "10: 23 14 | 28 1",
    "1: \"a\"",
    "11: 42 31",
    "5: 1 14 | 15 1",
    "19: 14 1 | 14 14",
    "12: 24 14 | 19 1",
    "16: 15 1 | 14 14",
    "31: 14 17 | 1 13",
    "6: 14 14 | 1 14",
    "2: 1 24 | 14 4",
    "0: 8 11",
    "13: 14 3 | 1 12",
    "15: 1 | 14",
    "17: 14 2 | 1 7",
    "23: 25 1 | 22 14",
    "28: 16 1",
    "4: 1 1",
    "20: 14 14 | 1 15",
    "3: 5 14 | 16 1",
    "27: 1 6 | 14 18",
    "14: \"b\"",
    "21: 14 1 | 1 14",
    "25: 1 1 | 1 14",
    "22: 14 14",
    "8: 42",
    "26: 14 22 | 1 20",
    "18: 15 15",
    "7: 14 5 | 1 21",
    "24: 14 1"
  ]

complicatedMessages =
  [ "abbbbbabbbaaaababbaabbbbabababbbabbbbbbabaaaa",
    "bbabbbbaabaabba",
    "babbbbaabbbbbabbbbbbaabaaabaaa",
    "aaabbbbbbaaaabaababaabababbabaaabbababababaaa",
    "bbbbbbbaaaabbbbaaabbabaaa",
    "bbbababbbbaaaaaaaabbababaaababaabab",
    "ababaaaaaabaaab",
    "ababaaaaabbbaba",
    "baabbaaaabbaaaababbaababb",
    "abbbbabbbbaaaababbbbbbaaaababb",
    "aaaaabbaabaaaaababaa",
    "aaaabbaaaabbaaa",
    "aaaabbaabbaaaaaaabbbabbbaaabbaabaaa",
    "babaaabbbaaabaababbaabababaaab",
    "aabbbbbaabbbaaaaaabbbbbababaaaaabbaaabba"
  ]

solve = do
  file <- T.splitOn "\n\n" <$> readInput 19
  let [rules, messages] = (fmap T.unpack) <$> T.splitOn "\n" <$> file
  print $ part1 rules messages
  print $ part2 rules messages
