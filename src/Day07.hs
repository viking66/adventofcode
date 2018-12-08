module Day07 (day07) where

import Control.Arrow (first)
import Data.Char (isAlpha, ord)
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

type Workers = Int

data Scheduler = Scheduler Workers Graph [(Int, Char)]

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
nextStep = firstStep . nextSteps
  where firstStep [] = Nothing
        firstStep (x:_) = Just x

nextSteps :: Graph -> [Char]
nextSteps (Graph xs m) = filter (flip Map.notMember m) xs

removeStep :: Char -> Graph -> Graph
removeStep s (Graph xs m) = Graph (f s xs) (g s m)
  where f x = filter (/= x)
        g x = Map.filter (not . null) . Map.map (f x)

stepTime :: Char -> (Int, Char)
stepTime c = ((ord c) - (ord 'A') + 61, c)

makeScheduler :: Workers -> Graph -> Scheduler
makeScheduler w g = let s = nextSteps g
                        t = take (min w (length s)) s
                        u = map stepTime t
                    in Scheduler w g u

filterRunning :: [(Int, Char)] -> [Char] -> [Char]
filterRunning running new = let r = map snd running
                            in filter (not . flip elem r) new

runScheduler :: Scheduler -> Int
runScheduler (Scheduler w g j) = run 0 w g j
  where run n w g j = let m = fst $ minimum j
                          j' = map (first (flip (-) m)) j
                          done = map snd $ filter ((== 0) . fst) j'
                          remaining = filter ((/= 0) . fst) j'
                          g' = foldr removeStep g done
                          next = map stepTime $ take (w - length remaining) $ filterRunning remaining (nextSteps g')
                          next' = remaining ++ next
                      in if null next'
                         then n + m
                         else run (n + m) w g' next'

go1 :: Graph -> [Char]
go1 (Graph [] _) = []
go1 g = let step = nextStep g
            update s = s : (go1 (removeStep s g))
       in maybe [] update step

go2 :: Graph -> Int
go2 = runScheduler . makeScheduler 5

go :: Graph -> ([Char], Int)
go g = (go1 g, go2 g)

day07 :: String -> Showable
day07 = maybe (pack "Unable to parse input") (pack . go . makeGraph) . traverse parseLine . lines
