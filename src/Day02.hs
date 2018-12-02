module Day02 (day02) where

import Data.List (group, sort)
import qualified Data.Set as Set

data ChecksumElem = Zero | Pair | Triplet | Both

idToChecksumElem :: String -> ChecksumElem
idToChecksumElem s = go (elem 2 lengths) (elem 3 lengths)
  where lengths = map length $ group $ sort s
        go False False = Zero
        go True False  = Pair
        go False True  = Triplet
        go True True   = Both

calcChecksum :: [ChecksumElem] -> Int
calcChecksum = uncurry (*) . foldr f (0, 0)
  where f Zero x = x
        f Pair (a, b) = (succ a, b)
        f Triplet (a, b) = (a, succ b)
        f Both (a, b) = (succ a, succ b)

drop1 :: String -> [(String, String)]
drop1 [] = []
drop1 xs = [drop1' i xs | i <- [0..length xs - 1]]
  where drop1' :: Int -> String -> (String, String)
        drop1' i xs = let (as, bs) = splitAt i xs
                      in case bs of
                           [] -> (as, bs)
                           (c:cs) -> (as, cs)

findMatch :: [(String, String)] -> Maybe (String, String)
findMatch = findMatch' Set.empty
  where findMatch' s [] = Nothing
        findMatch' s (x:xs) = if Set.member x s
                              then Just x
                              else findMatch' (Set.insert x s) xs

day02 :: String -> (Int, String)
day02 s = let xs = lines s
              checksum = calcChecksum $ map idToChecksumElem $ xs
              match = maybe "" (uncurry (++)) $ findMatch $ concatMap drop1 xs
          in (checksum, match)
