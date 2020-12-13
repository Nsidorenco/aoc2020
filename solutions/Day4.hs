{-# LANGUAGE OverloadedStrings #-}

module Day2 where

import Data.Char
import qualified Data.Text as T
import FileHelper

sampleInput =
  [ "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd",
    "byr:1937 iyr:2017 cid:147 hgt:183cm",
    "",
    "iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884",
    "hcl:#cfa07d byr:1929",
    "",
    "hcl:#ae17e1 iyr:2013",
    "eyr:2024",
    "ecl:brn pid:760753108 byr:1931",
    "hgt:179cm",
    "",
    "hcl:#cfa07d eyr:2025 pid:166559648",
    "iyr:2011 ecl:brn hgt:59in"
  ]

sampleInvalid =
  [ "eyr:1972 cid:100",
    "hcl:#18171d ecl:amb hgt:170 pid:186cm iyr:2018 byr:1926",
    "",
    "iyr:2019",
    "hcl:#602927 eyr:1967 hgt:170cm",
    "ecl:grn pid:012533040 byr:1946",
    "",
    "hcl:dab227 iyr:2012",
    "ecl:brn hgt:182cm pid:021572410 eyr:2020 byr:1992 cid:277",
    "",
    "hgt:59cm ecl:zzz",
    "eyr:2038 hcl:74454a iyr:2023",
    "pid:3556412378 byr:2007"
  ]

sampleValid =
  [ "pid:087499704 hgt:74in ecl:grn iyr:2012 eyr:2030 byr:1980",
    "hcl:#623a2f",
    "",
    "eyr:2029 ecl:blu cid:129 byr:1989",
    "iyr:2014 pid:896056539 hcl:#a97842 hgt:165cm",
    "",
    "hcl:#888785",
    "hgt:164cm byr:2001 iyr:2015 cid:88",
    "pid:545766238 ecl:hzl",
    "eyr:2022",
    "",
    "iyr:2010 hgt:158cm hcl:#b6652a ecl:blu byr:1944 eyr:2021 pid:093154719"
  ]

data Field
  = Byr String
  | Iyr String
  | Eyr String
  | Hgt String
  | Hcl String
  | Ecl String
  | Pid String
  | Cid String
  deriving (Show)

parseField :: String -> Field
parseField s =
  let (field, val) = break (== ':') s
   in case field of
        "byr" -> Byr $ tail val
        "iyr" -> Iyr $ tail val
        "eyr" -> Eyr $ tail val
        "hgt" -> Hgt $ tail val
        "hcl" -> Hcl $ tail val
        "ecl" -> Ecl $ tail val
        "pid" -> Pid $ tail val
        "cid" -> Cid $ tail val

parseEntry :: [String] -> ([Field], [String])
parseEntry [] = ([], [])
parseEntry ("" : ss) = ([], ss)
parseEntry (s : ss) =
  let ws = words s
      (flds, rest) = parseEntry ss
   in ((map parseField ws) ++ flds, rest)

parseEntries :: [String] -> [[Field]]
parseEntries [] = [[]]
parseEntries input =
  let (tpl, s) = parseEntry input
   in tpl : (parseEntries s)

requiredField :: Field -> Int
requiredField (Cid _) = 0
requiredField _ = 1

validateEntry :: [Field] -> Bool
validateEntry fields = 7 <= n
  where
    n = sum $ map requiredField fields

validateEntryFields :: Field -> Bool
validateEntryFields (Byr n) =
  let val = read n :: Int
   in 1920 <= val && val <= 2002
validateEntryFields (Iyr n) =
  let val = read n :: Int
   in 2010 <= val && val <= 2020
validateEntryFields (Eyr n) =
  let val = read n :: Int
   in 2020 <= val && val <= 2030
validateEntryFields (Hgt n) =
  let (v, unit) = span isDigit n
   in let val = read v :: Int
       in all isNumber v
            && case unit of
              "cm" -> 150 <= val && val <= 193
              "in" -> 59 <= val && val <= 76
              _ -> False
validateEntryFields (Hcl n) =
  head n == '#' && length (tail n) == 6
    && (all validColor (tail n))
  where
    validColor :: Char -> Bool
    validColor '0' = True
    validColor '1' = True
    validColor '2' = True
    validColor '3' = True
    validColor '4' = True
    validColor '5' = True
    validColor '6' = True
    validColor '7' = True
    validColor '8' = True
    validColor '9' = True
    validColor 'a' = True
    validColor 'b' = True
    validColor 'c' = True
    validColor 'd' = True
    validColor 'e' = True
    validColor 'f' = True
    validColor _ = False
validateEntryFields (Ecl n) =
  case n of
    "amb" -> True
    "blu" -> True
    "brn" -> True
    "gry" -> True
    "grn" -> True
    "hzl" -> True
    "oth" -> True
    otherwise -> False
validateEntryFields (Pid n) =
  length n == 9 && (all isNumber n)
validateEntryFields (Cid _) = True

part1 :: [[Field]] -> Int
part1 fields =
  length $ filter validateEntry fields

part2 :: [[Field]] -> Int
part2 fields =
  length $ filter (\x -> validateEntry x && (all validateEntryFields x)) fields

solve :: IO ()
solve = do
  file <- readInput 4
  let input = T.unpack <$> T.splitOn "\n" file
  print $ part1 $ parseEntries input
  print $ part2 $ parseEntries input

-- print $ (map (\x -> validateEntryFields x && validateEntry x)) $ parseEntries input
