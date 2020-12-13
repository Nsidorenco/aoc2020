{-# LANGUAGE OverloadedStrings #-}

module Day12 where

import qualified Data.Text as T
import FileHelper

sampleInput =
  [ "F10",
    "N3",
    "F7",
    "R90",
    "F11"
  ]

data Direction
  = North
  | West
  | South
  | East
  deriving (Eq, Show, Enum)

type Waypoint = (Int, Int)

-- rot is the rotational direction:
-- 1 is counter-clockwise and -1 is clockwise
rotate :: Direction -> Int -> Int -> Direction
rotate dir deg rot = newDir (fromEnum dir)
  where
    newDir i = cycle [North, West, South, East] !! ((4 + i) + (rot * step deg))
    step 90 = 1
    step 180 = 2
    step 270 = 3

moveDirection :: String -> (Direction, (Int, Int)) -> (Direction, (Int, Int))
moveDirection ins (dir, loc@(x, y)) =
  case head ins of
    'N' -> (dir, (x, y + amp))
    'S' -> (dir, (x, y - amp))
    'W' -> (dir, (x + amp, y))
    'E' -> (dir, (x - amp, y))
    'L' -> (rotate dir amp 1, loc)
    'R' -> (rotate dir amp (-1), loc)
    'F' -> case dir of
      North -> (dir, (x, y + amp))
      South -> (dir, (x, y - amp))
      West -> (dir, (x + amp, y))
      East -> (dir, (x - amp, y))
  where
    amp = read (tail ins) :: Int

manhattanDist :: (Int, Int) -> (Int, Int) -> Int
manhattanDist (x, y) (x', y') = abs (x - x') + abs (y - y')

part1 :: [String] -> Int
part1 input =
  let (_, loc) = foldl (flip moveDirection) (East, (0, 0)) input
   in manhattanDist (0, 0) loc

rotateWaypointLeft :: Waypoint -> Int -> Waypoint
rotateWaypointLeft wp deg =
  foldl (\acc _ -> rotateOnce acc) wp [1 .. step deg]
  where
    step 90 = 1
    step 180 = 2
    step 270 = 3
    rotateOnce (dx, dy)
      | dx < 0 && dy < 0 = (dy, - dx)
      | dy < 0 = (dy, - dx) -- dx pos
      | dx < 0 = (dy, - dx) -- dy pos
      | otherwise = (dy, - dx)

rotateWaypointRight :: Waypoint -> Int -> Waypoint
rotateWaypointRight wp deg =
  foldl (\acc _ -> rotateOnce acc) wp [1 .. step deg]
  where
    step 90 = 1
    step 180 = 2
    step 270 = 3
    rotateOnce (dx, dy)
      | dx < 0 && dy < 0 = (- dy, dx)
      | dy < 0 = (- dy, dx) -- dx pos
      | dx < 0 = (- dy, dx) -- dy pos
      | otherwise = (- dy, dx)

-- East 10, North 1 = (-10, 1)
-- RotateOnce -> (1, -10)
-- West 10, South 1 = (10, -1)
-- RotateOnce -> (1, 10)

moveWaypoint :: String -> (Waypoint, (Int, Int)) -> (Waypoint, (Int, Int))
moveWaypoint ins (wp@(dx, dy), loc@(x, y)) =
  case head ins of
    'N' -> ((dx, dy + amp), loc)
    'S' -> ((dx, dy - amp), loc)
    'W' -> ((dx + amp, dy), loc)
    'E' -> ((dx - amp, dy), loc)
    'L' -> (rotateWaypointLeft wp amp, loc)
    'R' -> (rotateWaypointRight wp amp, loc)
    'F' -> (wp, (x + dx * amp, y + dy * amp))
  where
    amp = read (tail ins) :: Int

part2 :: [String] -> Int
part2 input =
  let (_, loc) = foldl (flip moveWaypoint) ((-10, 1), (0, 0)) input
   in manhattanDist (0, 0) loc

solve :: IO ()
solve = do
  file <- readInput 12
  let input = map T.unpack $ T.splitOn "\n" file
  print $ part1 input
  print $ part2 input

--   print $ part2 input
