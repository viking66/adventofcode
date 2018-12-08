module Solutions
  ( day01
  , day02
  , day03
  , day04
  , day05
  , day06
  , day07
  , day08
  -- , day09
  -- , day10
  -- , day11
  -- , day12
  -- , day13
  -- , day14
  -- , day15
  -- , day16
  -- , day17
  -- , day18
  -- , day19
  -- , day20
  -- , day21
  -- , day22
  -- , day23
  -- , day24
  -- , day25
  , getSolution
  , putSolution
  , solutionMap
  ) where

import Prelude hiding (lookup)

import Data.Map.Lazy (fromList, lookup, Map)
import Text.Printf (printf)

import Day01 (day01)
import Day02 (day02)
import Day03 (day03)
import Day04 (day04)
import Day05 (day05)
import Day06 (day06)
import Day07 (day07)
import Day08 (day08)
-- import Day09 (day09)
-- import Day10 (day10)
-- import Day11 (day11)
-- import Day12 (day12)
-- import Day13 (day13)
-- import Day14 (day14)
-- import Day15 (day15)
-- import Day16 (day16)
-- import Day17 (day17)
-- import Day18 (day18)
-- import Day19 (day19)
-- import Day20 (day20)
-- import Day21 (day21)
-- import Day22 (day22)
-- import Day23 (day23)
-- import Day24 (day24)
-- import Day25 (day25)

import Types

solutionMap :: Map Int (String -> Showable)
solutionMap = fromList
  [ (1, day01), (2, day02), (3, day03), (4, day04), (5, day05), (6, day06)
  , (7, day07), (8, day08)
  -- , (9, day09)
  -- , (10, day10)
  -- , (11, day11)
  -- , (12, day12)
  -- , (13, day13)
  -- , (14, day14)
  -- , (15, day15)
  -- , (16, day16)
  -- , (17, day17)
  -- , (18, day18)
  -- , (19, day19)
  -- , (20, day20)
  -- , (21, day21)
  -- , (22, day22)
  -- , (23, day23)
  -- , (24, day24)
  -- , (25, day25)
  ]

getFile :: Int -> String
getFile = printf "data/day%02d.txt"

getSolution :: Maybe Int -> IO Showable
getSolution = maybe noSol getSolution'
  where noSol :: IO Showable
        noSol = pure $ pack "No solution available"
        eval :: Int -> (String -> Showable) -> IO Showable
        eval n f = readFile (getFile n) >>= pure . f
        getSolution' :: Int -> IO Showable
        getSolution' n = maybe noSol (eval n) (lookup n solutionMap)

putSolution :: Maybe Int -> Showable -> IO ()
putSolution Nothing s = putStrLn $ show s
putSolution (Just n) s = putStrLn $ show n ++ ": " ++ show s

