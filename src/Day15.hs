module Day15 (day15) where

import Control.Applicative hiding (many)
import Data.List (sort)
import Data.Set (Set)
import qualified Data.Set as Set

import Parser
import Types

data Coord = Coord Int Int
  deriving (Show, Eq)

data CaveMap = Elf' Coord | Goblin' Coord | Path' Coord | Wall' Coord
  deriving Show

data Elf = Elf { eLocation :: Coord
               , eAttack :: Int
               , eHit :: Int
               }
  deriving (Eq, Show)

data Goblin = Goblin { gLocation :: Coord
                     , gAttack :: Int
                     , gHit :: Int
                     }
  deriving (Eq, Show)

data CaveState = CaveState { paths :: Set Coord
                           , elves :: Set Elf
                           , goblins :: Set Goblin
                           }
  deriving Show

data Creature = Bad Goblin | Good Elf
  deriving (Eq, Show)

instance Ord Coord where
  compare (Coord x y) (Coord a b) = compare y b <> compare x a

instance Ord Elf where
  compare (Elf a _ _) (Elf b _ _) = compare a b

instance Ord Goblin where
  compare (Goblin a _ _) (Goblin b _ _) = compare a b

instance Ord Creature where
  compare c1 c2 = compare (getCreatureCoord c1) (getCreatureCoord c2)

getCreatureCoord :: Creature -> Coord
getCreatureCoord (Good (Elf a _ _)) = a
getCreatureCoord (Bad (Goblin a _ _)) = a

elementParser :: (Coord -> CaveMap) -> Char -> Coord -> Parser CaveMap
elementParser t ch co = const (t co) <$> char ch

elfParser :: Coord -> Parser CaveMap
elfParser = elementParser Elf' 'E'

goblinParser :: Coord -> Parser CaveMap
goblinParser = elementParser Goblin' 'G'

pathParser :: Coord -> Parser CaveMap
pathParser = elementParser Path' '.'

wallParser :: Coord -> Parser CaveMap
wallParser = elementParser Wall' '#'

caveMapParser :: Coord -> Parser CaveMap
caveMapParser c = elfParser c <|> goblinParser c <|> pathParser c <|> wallParser c

size :: [[a]] -> (Int, Int)
size xss = (maximum $ map length xss, length xss)

coords :: (Int, Int) -> [Coord]
coords (x, y) = [Coord x' y' | y' <- [0..y-1], x' <- [0..x-1]]

sortedCreatuers :: CaveState -> [Creature]
sortedCreatuers (CaveState _ e g) = sort $ (map Good $ Set.toList e) ++ (map Bad $ Set.toList g)

isBad :: Creature -> Bool
isBad (Bad _) = True
isBad _ = False

isGood :: Creature -> Bool
isGood (Good _) = True
isGood _ = False

makeElf :: Coord -> Elf
makeElf c = Elf c 3 200

makeGoblin :: Coord -> Goblin
makeGoblin c = Goblin c 3 200

makeCave :: CaveMap -> CaveState -> CaveState
makeCave (Elf' c) (CaveState p e g) = CaveState (Set.insert c p) (Set.insert (makeElf c) e) g
makeCave (Goblin' c) (CaveState p e g) = CaveState (Set.insert c p) e (Set.insert (makeGoblin c) g)
makeCave (Path' c) (CaveState p e g) = CaveState (Set.insert c p) e g
makeCave _ s = s

isDone :: CaveState -> Bool
isDone c = let c' = sortedCreatuers c
               g = filter isGood c'
               b = filter isBad c'
           in null g || null b

takeTurn :: CaveState -> Creature -> CaveState
takeTurn c _ = c
-- takeTurn state creature =
--   let f = if isGood creature then isBad else isGood
--       targets = filter f sortedCreatures state
--   in _todo

go :: CaveState -> CaveState
go c
  | isDone c  = c
  | otherwise = go $ foldl takeTurn c (sortedCreatuers c)

setup :: String -> Maybe CaveState
setup xs = let parsers = map (\c -> caveMapParser c) $ coords (size $ lines xs)
               parsedInput = runParser (sequenceParser parsers) (filter (/= '\n') xs)
               cave = CaveState Set.empty Set.empty Set.empty
           in (foldr makeCave cave . fst) <$> parsedInput

day15 :: String -> Showable
day15 = maybe (pack "Failed to parse input") (pack . go) . setup
