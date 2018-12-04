module Day03 (day03) where

import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as Map

import Parser
import Types

data Claim = Claim { idx :: Int
                   , xInit :: Int
                   , yInit :: Int
                   , xLen :: Int
                   , yLen :: Int
                   }
  deriving Show

parseLine :: String -> Maybe Claim
parseLine s = fst <$> runParser claimParser s
  where idxParse = char '#' *> number <* (spaces *> char '@' *> spaces)
        xInitXParse = number <* char ','
        yInitParse = number <* (char ':' *> spaces)
        xLenParse = number <* char 'x'
        yLenParse = number
        claimParser = Claim <$> idxParse <*> xInitXParse <*> yInitParse <*> xLenParse <*> yLenParse

parseInput :: String -> Maybe [Claim]
parseInput = traverse parseLine . lines

claimIdxs :: Claim -> [(Int, Int)]
claimIdxs (Claim _ x y xl yl) = [(a,b) | a <- [x..x+xl-1], b <- [y..y+yl-1]]

claimIdxMap :: [Claim] -> Map (Int, Int) Int
claimIdxMap = foldr f Map.empty . concatMap claimIdxs
  where f k = Map.insertWithKey g k 1
        g _ new old = new + old

countOverlap :: [Claim] -> Int
countOverlap = length . filter (>1) . Map.elems . claimIdxMap

day03 :: String -> Showable
day03 = maybe (pack "Failed to parse input") (pack . countOverlap) . parseInput
