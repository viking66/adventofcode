module Day05 (day05) where

import Data.Char (toLower, toUpper)
import Data.Set (Set)
import qualified Data.Set as Set

import Types

trigger :: Char -> Char -> Bool
trigger a b = a /= b && toUpper a == toUpper b

react :: String -> String
react (x:y:zs) = if trigger x y then react zs else x : (react (y:zs))
react xs = xs

resultLength :: String -> Int
resultLength s = let xs = iterate react s
                 in length $ fst $ head $ dropWhile (uncurry (/=)) $ zip xs (tail xs)

getPolymers :: String -> String
getPolymers = Set.elems . Set.fromList . map toUpper

filterPolymer :: String -> Char -> String
filterPolymer s c = let a = toLower c
                        b = toUpper c
                    in filter (\c' -> c' /= a && c' /= b) s

solve :: String -> (Int, Int)
solve s = let s' = filter (/= '\n') s
              xs = map (filterPolymer s') $ getPolymers s'
          in (resultLength s', minimum (map resultLength xs))

day05 :: String -> Showable
day05 = pack . solve
