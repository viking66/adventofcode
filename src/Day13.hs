module Day13 (day13) where

import Control.Applicative hiding (many)
import Data.List (sort)
import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as Map

import Parser
import Types

data Path = NS | EW | RP | LP | Intersection | Empty

data Cart = NCart | SCart | ECart | WCart
  deriving Eq

data TrackElement = TrackPath Path | TrackCart Cart

data Turn = LTurn | Straight | RTurn
  deriving (Eq, Show)

data Coord = Coord Int Int
  deriving (Show, Eq)

data CartNav = CartNav Cart Coord [Turn] Coord
  deriving Eq

data Track = Track (Map Coord Path) [CartNav]
  deriving Show

data TickResult = Crash Track [(Coord, Coord)] | TickResult Track

instance Show Path where
  show NS = "|"
  show EW = "-"
  show RP = "/"
  show LP = "\\"
  show Intersection = "+"
  show Empty = " "

instance Show Cart where
  show NCart = "^"
  show SCart = "v"
  show ECart = ">"
  show WCart = "<"

instance Show TrackElement where
  show (TrackPath p) = show p
  show (TrackCart c) = show c

instance Show CartNav where
  show (CartNav cart coord turns i) =
    "CartNav " ++ (show cart) ++ " " ++ show coord ++ " " ++ show (take 3 turns) ++ " Id: " ++ (show i)

instance Ord Coord where
  compare (Coord x y) (Coord a b) = compare y b <> compare x a

instance Ord CartNav where
  compare (CartNav _ a _ _ ) (CartNav _ b _ _) = compare a b

parsePath :: Parser Path
parsePath = const NS <$> char '|'
        <|> const EW <$> char '-'
        <|> const RP <$> char '/'
        <|> const LP <$> char '\\'
        <|> const Intersection <$> char '+'
        <|> const Empty <$> char ' '

parseCart :: Parser Cart
parseCart = const NCart <$> char '^'
        <|> const SCart <$> char 'v'
        <|> const ECart <$> char '>'
        <|> const WCart <$> char '<'

parseTrackElement :: Parser TrackElement
parseTrackElement = (TrackPath <$> parsePath)
                <|> (TrackCart <$> parseCart)

size :: [[a]] -> (Int, Int)
size xss = (maximum $ map length xss, length xss)

coords :: (Int, Int) -> [Coord]
coords (x, y) = [Coord x' y' | y' <- [0..y-1], x' <- [0..x-1]]

north :: Coord -> Coord
north (Coord x y) = Coord x (y-1)

south :: Coord -> Coord
south (Coord x y) = Coord x (y+1)

east :: Coord -> Coord
east (Coord x y) = Coord (x+1) y

west :: Coord -> Coord
west (Coord x y) = Coord (x-1) y

intersection :: CartNav -> CartNav
intersection c@(CartNav _ _ [] _) = c
intersection (CartNav NCart coord (LTurn:xs) i) = CartNav WCart (west coord) xs i
intersection (CartNav NCart coord (Straight:xs) i) = CartNav NCart (north coord) xs i
intersection (CartNav NCart coord (RTurn:xs) i) = CartNav ECart (east coord) xs i
intersection (CartNav SCart coord (LTurn:xs) i) = CartNav ECart (east coord) xs i
intersection (CartNav SCart coord (Straight:xs) i) = CartNav SCart (south coord) xs i
intersection (CartNav SCart coord (RTurn:xs) i) = CartNav WCart (west coord) xs i
intersection (CartNav ECart coord (LTurn:xs) i) = CartNav NCart (north coord) xs i
intersection (CartNav ECart coord (Straight:xs) i) = CartNav ECart (east coord) xs i
intersection (CartNav ECart coord (RTurn:xs) i) = CartNav SCart (south coord) xs i
intersection (CartNav WCart coord (LTurn:xs) i) = CartNav SCart (south coord) xs i
intersection (CartNav WCart coord (Straight:xs) i) = CartNav WCart (west coord) xs i
intersection (CartNav WCart coord (RTurn:xs) i) = CartNav NCart (north coord) xs i

newLocation :: Map Coord Path -> CartNav -> CartNav
newLocation m n@(CartNav cart coord turns i) =
  case (Map.findWithDefault Empty coord m, cart) of
    (NS, NCart) -> CartNav NCart (north coord) turns i
    (NS, SCart) -> CartNav SCart (south coord) turns i
    (EW, ECart) -> CartNav ECart (east coord) turns i
    (EW, WCart) -> CartNav WCart (west coord) turns i
    (RP, NCart) -> CartNav ECart (east coord) turns i
    (RP, SCart) -> CartNav WCart (west coord) turns i
    (RP, ECart) -> CartNav NCart (north coord) turns i
    (RP, WCart) -> CartNav SCart (south coord) turns i
    (LP, NCart) -> CartNav WCart (west coord) turns i
    (LP, SCart) -> CartNav ECart (east coord) turns i
    (LP, ECart) -> CartNav SCart (south coord) turns i
    (LP, WCart) -> CartNav NCart (north coord) turns i
    (Intersection, _) -> intersection n
    (_, _) -> n

findCrashes :: [(CartNav, [CartNav])] -> [(Coord, Coord)]
findCrashes xs = (midTickCrash xs) ++ (postTickCrash (map fst xs))
  where midTickCrash = foldl f []
        f ids (c, cs) = crashIds c cs ++ ids
        postTickCrash [] = []
        postTickCrash (x:xs) = (crashIds x xs) ++ (postTickCrash xs)
        crashIds _ [] = []
        crashIds x@(CartNav _ c _ i) ((CartNav _ c' _ i'):xs) =
          if c /= c' then crashIds x xs else (i,c):(i',c):(crashIds x xs)

notCrash :: TickResult -> Bool
notCrash (TickResult _) = True
notCrash _ = False

showResult :: TickResult -> Showable
showResult (TickResult x) = pack x
showResult (Crash _ []) = pack "Unexpected state"
showResult (Crash _ ((_, x):_)) = pack x

tick :: TickResult -> TickResult
tick c@(Crash _ _) = c
tick (TickResult (Track m xs)) = let moves = tick' m (sort xs)
                                     track = Track m (map fst moves)
                                     crashes = findCrashes moves
                                 in if null crashes
                                    then (TickResult track)
                                    else Crash track crashes
  where tick' _ [] = []
        tick' m (x:xs) = (newLocation m x, xs) : (tick' m xs)

initTrack :: [(Coord, TrackElement)] -> Track
initTrack = foldr f (Track Map.empty [])
  where turns = cycle [LTurn, Straight, RTurn]
        f (c, (TrackPath p)) (Track m xs) = Track (Map.insert c p m) xs
        f (c, (TrackCart NCart)) (Track m xs) = Track (Map.insert c NS m) (CartNav NCart c turns c : xs)
        f (c, (TrackCart SCart)) (Track m xs) = Track (Map.insert c NS m) (CartNav SCart c turns c : xs)
        f (c, (TrackCart ECart)) (Track m xs) = Track (Map.insert c EW m) (CartNav ECart c turns c : xs)
        f (c, (TrackCart WCart)) (Track m xs) = Track (Map.insert c EW m) (CartNav WCart c turns c : xs)

getLastCart :: TickResult -> Coord
getLastCart t@(TickResult _) = getLastCart (tick t)
getLastCart (Crash _ []) = Coord 0 0
getLastCart (Crash _ [(_,x)]) = x
getLastCart (Crash (Track m ys) xs) = case filter f ys of
                                          [(CartNav _ c _ _)] -> c
                                          ys' -> getLastCart $ TickResult $ Track m ys'
  where ids = map fst xs
        f (CartNav _ _ _ i) = i `notElem` ids

go :: [String] -> Showable
go xs = let cs = coords (size xs)
            ts = runParser (many parseTrackElement) (concat xs)
            ts' = maybe [] fst ts
            ticks = iterate tick $ TickResult $ initTrack (zip cs ts')
            a = showResult $ head $ dropWhile notCrash ticks
            b = getLastCart $ head ticks
        in pack (a, b)

day13 :: String -> Showable
day13 = go . lines
