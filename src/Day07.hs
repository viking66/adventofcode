module Day07 (day07) where

import Data.Char (isAlpha)
import Data.List (sort)
import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as Map
import qualified Data.Set as Set

import Parser
import Types

data Dependency = Dependency Char Char
  deriving Show

data Graph = Graph [Char] (Map Char [Char])
  deriving Show

-- Step C must be finished before step A can begin.
parseLine :: String -> Maybe Dependency
parseLine = fmap fst . runParser parseLine'
  where parseLine' = Dependency <$> (string "Step " *> isChar)
                                <*> (string " must be finished before step "
                                     *> isChar
                                     <* string " can begin.")
        isChar = satisfy isAlpha

nodes :: [Dependency] -> [Char]
nodes = sort . Set.elems . foldr f Set.empty
  where f (Dependency a b) = Set.insert b . Set.insert a

dependencyMap :: [Dependency] -> Map Char [Char]
dependencyMap = foldr f Map.empty
  where f (Dependency a b) = Map.insertWith (++) b [a]

makeGraph :: [Dependency] -> Graph
makeGraph xs = Graph (nodes xs) (dependencyMap xs)

nextStep :: Graph -> Maybe Char
nextStep (Graph xs m) = nextStep' xs m
  where nextStep' (x:xs) m = if Map.notMember x m then Just x else nextStep' xs m
        nextStep' [] _ = Nothing

removeStep :: Char -> Graph -> Graph
removeStep s (Graph xs m) = Graph (f s xs) (g s m)
  where f x = filter (/= x)
        g x = Map.filter (not . null) . Map.map (f x)

go :: Graph -> [Char]
go (Graph [] _) = []
go g = let step = nextStep g
           update s = s : (go (removeStep s g))
       in maybe [] update step

day07 :: String -> Showable
day07 = maybe (pack "Unable to parse input") (pack . go . makeGraph) . traverse parseLine . lines
