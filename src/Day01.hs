module Day01 (day01) where

import Data.Foldable (fold)
import Data.Monoid (Sum(..))
import Data.Set (Set)
import qualified Data.Set as Set

parseVal :: String -> Sum Integer
parseVal ('+':s) = parseVal s
parseVal s = Sum $ read s

findRepeat :: (Monoid a, Ord a) => [a] -> a
findRepeat = findRepeat' Set.empty . scanl (<>) mempty . cycle
  where findRepeat' s [] = mempty
        findRepeat' s (x:xs) = if Set.member x s
                               then x
                               else findRepeat' (Set.insert x s) xs

day01 :: String -> (Integer, Integer)
day01 s = let vals = map parseVal $ lines s
          in (getSum (fold vals), getSum (findRepeat vals))
