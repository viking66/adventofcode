module Day06 (day06) where

import Data.List (group, sort)
import Data.Set (Set)
import qualified Data.Set as Set

import Parser
import Types

parseInput :: [String] -> Maybe [(Int, Int)]
parseInput = traverse parsePair
  where intPair = ((,)) <$> (number <* string ", ") <*> number
        parsePair s = fst <$> runParser intPair s

boundries :: [(Int, Int)] -> (Int, Int, Int, Int)
boundries ns = let xs = map fst ns
                   ys = map snd ns
               in (minimum xs, maximum xs, minimum ys, maximum ys)

exterior :: (Int, Int, Int, Int) -> [(Int, Int)]
exterior (xMin, xMax, yMin, yMax) =
  let as = [(x,y) | x <- [xMin, xMax], y <- [yMin..yMax]]
      bs = [(x,y) | x <- [xMin..xMax], y <- [yMin, yMax]]
  in as ++ bs

interior :: (Int, Int, Int, Int) -> [(Int, Int)]
interior (xMin, xMax, yMin, yMax) =
  [(x, y) | x <- [xMin+1..xMax-1], y <- [yMin+1..yMax-1]]

distance :: (Int, Int) -> (Int, Int) -> Int
distance (x1, y1) (x2, y2) = abs (x1-x2) + abs (y1-y2)

closest :: [(Int, (Int, Int))] -> (Int, Int) -> Maybe Int
closest xs p = getMin $ map (f p) xs
  where f q (i, r) = (distance q r, i)
        minDist = fst . minimum
        distFilter n = filter (\(d, _) -> d == n)
        extractIndex [(_,i)] = Just i
        extractIndex _ = Nothing
        getMin ys = extractIndex $ distFilter (minDist ys) ys

unwrap :: [Maybe Int] -> [Int]
unwrap = map (maybe 0 id) . filter isJust
  where isJust = maybe False (const True)

filterPoints :: Set Int -> [Int] -> [Int]
filterPoints s = filter (flip Set.notMember s)

countRegion :: [(Int, Int)] -> [(Int, Int)] -> Int
countRegion xs = length . filter (< 10000) . map f
  where f a = sum $ map (distance a) xs

go :: [(Int, Int)] -> (Int, Int)
go xs = let xs' = zip [0..] xs
            b = boundries xs
            is = unwrap $ map (closest xs') (interior b)
            es = Set.fromList $ unwrap $ map (closest xs') (exterior b)
            safe1 = maximum $ map length $ group $ sort $ filterPoints es is
            safe2 = countRegion xs (interior b)
        in (safe1, safe2)

day06 :: String -> Showable
day06 = maybe (pack "Failed to parse input") (pack . go) . parseInput . lines
