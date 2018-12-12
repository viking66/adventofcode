module Day11 (day11) where

import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as Map

import Types

grid :: [(Int, Int)]
grid = [(x, y) | x <- [1..300], y <- [1..300]]

getPower :: Int -> (Int, Int) -> ((Int, Int), Int)
getPower serial (x, y) =
  let rackId = x + 10
      power = (((rackId * y + serial) * rackId) `div` 100 `mod` 10) - 5
  in ((x, y), power)

powerLevels :: Int -> [(Int, Int)] -> Map (Int, Int) Int
powerLevels serial = Map.fromList . map (getPower serial)

powerLevels' :: Map (Int, Int) Int -> Map (Int, Int, Int) Int
powerLevels' = Map.mapKeys (\(x, y) -> (x, y, 1))

valid :: (Int, Int, Int) -> Bool
valid (x, y, n)
  | x + n - 1 > 300 = False
  | y + n - 1 > 300 = False
  | otherwise       = True

insertPower :: (Int, Int, Int) -> Map (Int, Int, Int) Int -> Map (Int, Int, Int) Int
insertPower a@(x, y, n) m = if valid a
                            then Map.insert a power m
                            else m
  where targets = (x+1, y+1, n-1)
                : [(x, y',1) | y' <- [y..y+n-1]]
                ++ [(x', y,1) | x' <- [x+1..x+n-1]]
        power = sum $ map (flip (Map.findWithDefault 0) m) targets

powerSquares :: Map (Int, Int) Int -> Map (Int, Int, Int) Int
powerSquares ps = let ps' = powerLevels' ps
                      xs = [(x, y, n) | n <- [2..300], x <- [1..300], y <- [1..300], x+n <= 301 && y+n <= 301]
                  in foldl (flip insertPower) ps' xs

maxPowerSquare :: Map (Int, Int, Int) Int -> (Int, Int, Int)
maxPowerSquare = snd . Map.foldrWithKey f (0, (0, 0, 0))
  where f k' p' (p, k) = if p' > p then (p', k') else (p, k)

powerFold :: Map (Int, Int) Int -> Int -> (Int, Int) -> (Int, Int, Int, Int) -> (Int, Int, Int, Int)
powerFold m n (x, y) a@(_, _, p, _) =
  if x+n > 300 || y+n > 300
  then a
  else let xs = [(x+a, y+b) | a <- [0..n-1], b <- [0..n-1]]
           p' = sum $ map (flip (Map.findWithDefault 0) m) xs
       in if p' > p then (x, y, p', n) else a

go :: Int -> ((Int, Int), (Int, Int, Int))
go serial =
  let power = powerLevels serial grid
      coords = [(x, y) | x <- [1..300], y <- [1..300]]
      (x, y, _, _) = foldr (powerFold power 3) (0,0,0,0) coords
      p2 = maxPowerSquare $ powerSquares power
  in ((x, y), p2)

parseInput :: String -> Int
parseInput = read . filter (/= '\n')

day11 :: String -> Showable
day11 = pack . go . parseInput
