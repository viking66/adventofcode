module Day12 (day12) where

import Control.Applicative hiding (many)
import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as Map

import Parser
import Types

data PlantState = NoPlant | Plant
  deriving (Eq, Ord, Show)
data PlantPosition = PlantPosition Int PlantState
  deriving Show
data Pattern = Pattern PlantState PlantState PlantState PlantState PlantState
  deriving (Eq, Ord, Show)
data Generation = Generation (Map Pattern PlantState) [PlantPosition]
  deriving Show

plantState :: Parser PlantState
plantState = (const NoPlant <$> char '.')
         <|> (const Plant <$> char '#')

parsePlantState :: String -> [PlantPosition]
parsePlantState = zipWith PlantPosition [0..] . maybe [] fst . runParser (header *> many plantState)
  where header = string "initial state: "

parseRule :: String -> Maybe (Pattern, PlantState)
parseRule = (fst <$>) . runParser rule
  where pattern = Pattern <$> plantState
                          <*> plantState
                          <*> plantState
                          <*> plantState
                          <*> plantState
        conclusion = string " => " *> plantState
        rule = ((,)) <$> pattern <*> conclusion

parseInput :: String -> Generation
parseInput s = let (x:_:zs) = lines s
                   state = parsePlantState x
                   rules = Map.fromList $ maybe [] id $ traverse parseRule zs
               in normalize (Generation rules state)

plant :: PlantPosition -> Bool
plant (PlantPosition _ Plant) = True
plant _ = False

notPlant :: PlantPosition -> Bool
notPlant (PlantPosition _ NoPlant) = True
notPlant _ = False

position :: PlantPosition -> Int
position (PlantPosition n _) = n

currentIndex :: [PlantPosition] -> Int
currentIndex ((PlantPosition n _):_) = n
currentIndex _ = 0

getPlantState :: PlantPosition -> PlantState
getPlantState (PlantPosition _ s) = s

trimFront :: [PlantPosition] -> [PlantPosition]
trimFront ps = let ps' = dropWhile notPlant ps
                   i = currentIndex ps'
                   p = PlantPosition (i-1) NoPlant
                   q = PlantPosition (i-2) NoPlant
                   r = PlantPosition (i-3) NoPlant
                   s = PlantPosition (i-4) NoPlant
                   t = PlantPosition (i-5) NoPlant
               in t:s:r:q:p:ps'

trimBack :: [PlantPosition] -> [PlantPosition]
trimBack ps = let ps' = dropWhile notPlant $ reverse ps
                  i = currentIndex ps'
                  p = PlantPosition (i+1) NoPlant
                  q = PlantPosition (i+2) NoPlant
                  r = PlantPosition (i+3) NoPlant
                  s = PlantPosition (i+4) NoPlant
                  t = PlantPosition (i+5) NoPlant
               in reverse $ t:s:r:q:p:ps'

toPattern :: [PlantState] -> Pattern
toPattern [a,b,c,d,e] = Pattern a b c d e
toPattern _ = Pattern NoPlant NoPlant NoPlant NoPlant NoPlant 

makePatterns :: [PlantPosition] -> [(Int, Pattern)]
makePatterns xs = if length xs < 5
                  then []
                  else let ys = take 5 xs
                           ys' = toPattern $ map getPlantState ys
                           i = currentIndex (drop 2 ys)
                       in (i, ys') : (makePatterns (tail xs))

normalize :: Generation -> Generation
normalize (Generation rs ps) = Generation rs (trimFront $ trimBack $ ps)

nextGeneration :: Generation -> Generation
nextGeneration (Generation m xs) =
  let xs' = map (f m) (makePatterns xs)
      f m (i, p) = PlantPosition i $ Map.findWithDefault NoPlant p m
  in normalize (Generation m xs')

getPlantPositions :: Generation -> [PlantPosition]
getPlantPositions (Generation _ ps) = ps

plantNumberSum :: Generation -> Int
plantNumberSum = sum . map position . filter plant . getPlantPositions

stablePoint :: Generation -> (Int, Int)
stablePoint g =
  let ns = map plantNumberSum $ iterate nextGeneration g
      diffs = zipWith (-) (tail ns) ns
      diffs' = zip [1..] $ zip diffs (tail diffs)
      diffEq (_, (a, b)) = a == b
      result (n, (a, _)) = (n, a)
  in result $ head $ dropWhile (not . diffEq) diffs'

go :: Generation -> (Int, Int)
go g = let a = go' 20 g
           (i, n) = stablePoint g
           b = (go' i g) + ((50000000000 - i) * n)
       in (a, b)
  where go' n = plantNumberSum . head . drop n . iterate nextGeneration

day12 :: String -> Showable
day12 = pack . go . parseInput
