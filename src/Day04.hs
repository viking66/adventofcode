module Day04 (day04) where

import Control.Applicative
import Data.Function (on)
import Data.List (reverse, sortBy)
import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as Map

import Parser
import Types

-- This solution needs to be refactored and cleaned up. Lots of stuff can be
-- refactored for better code reuse. 

type ETime = Int
type EId = Int

data EType = ClockIn EId
           | Sleep
           | Wake
  deriving Show

data EType' = Sleep' EId
            | Wake' EId
  deriving Show

data Event = Event ETime EType
           | Event' ETime EType'
  deriving Show

getEventTime :: Event -> ETime
getEventTime (Event t _) = t
getEventTime (Event' t _) = t

-- [1518-07-26 23:50] Guard #487 begins shift
-- [1518-06-22 00:48] wakes up
-- [1518-08-21 00:30] falls asleep
parseLine :: String -> Maybe Event
parseLine s = fst <$> runParser eventParser s
  where eventParser = Event <$> parseDateTime <*> parseEvent
        ydm y d m = y ++ d ++ m
        t h m    = h ++ m
        parseDateTime = read <$> ((++) <$> (char '[' *> parseDate)
                                       <*> (parseTime <* (char ']' <* spaces)))
        parseDate = ydm <$> (digits <* char '-')
                        <*> (digits <* char '-')
                        <*> (digits <* spaces)
        parseTime = t <$> digits <*> (char ':' *> digits)
        parseEvent = parseSleep <|> parseWake <|> parseClockIn
        parseSleep = const Sleep <$> string "falls asleep"
        parseWake = const Wake <$> string "wakes up"
        parseClockIn = ClockIn <$> (string "Guard #" *> number <* string " begins shift")

parseInput :: String -> Maybe [Event]
parseInput s = sortBy (compare `on` getEventTime) <$> (traverse parseLine $ lines s)

tagEId :: [Event] -> [Event]
tagEId = reverse . snd . foldl f (0, [])
  where f (_, xs) (Event _ (ClockIn eid)) = (eid, xs)
        f (eid, xs) (Event t Sleep) = (eid, (Event' t (Sleep' eid)) : xs)
        f (eid, xs) (Event t Wake) = (eid, (Event' t (Wake' eid)) : xs)
        f _ _ = (0,[])

getId :: Event -> EId
getId (Event _ _) = 0
getId (Event' _ (Sleep' eid)) = eid
getId (Event' _ (Wake' eid)) = eid

getTime :: Event -> ETime
getTime (Event t _) = t
getTime (Event' t _) = t

getMin :: ETime -> Int
getMin = flip mod 100

groupSleepWake :: [Event] -> [(EId, ETime, ETime)]
groupSleepWake xs = if length xs `div` 2 == 1 then [] else go xs
  where go [] = []
        go [_] = []
        go (x:y:zs) = (getId x, getMin (getTime x), getMin (getTime y)) : go zs

sleepBits :: (EId, ETime, ETime) -> (EId, [Int])
sleepBits (e, s, t) =
  let bits = replicate s 0 ++ replicate (t-s) 1 ++ replicate (60-t) 0
  in (e, bits)

reduceMap :: ([Int] -> Int) -> Map EId [Int] -> EId
reduceMap g = snd . maximum . Map.foldrWithKey f []
  where f :: EId -> [Int] -> [(Int, EId)] -> [(Int, EId)]
        f k x xs = (g x, k) : xs

sleepSumMax :: Map EId [Int] -> EId
sleepSumMax = reduceMap sum

sleepMinuteMax :: Map EId [Int] -> EId
sleepMinuteMax = reduceMap maximum

maxMinute :: EId -> Map EId [Int] -> Int
maxMinute e = maybe 0 f . Map.lookup e
  where f = snd . maximum . flip zip [0..]

go :: [Event] -> (Int, Int)
go xs = let m = Map.fromListWith (zipWith (+)) $ map sleepBits $ groupSleepWake $ tagEId xs
            eid1 = sleepSumMax m
            minute1 = maxMinute eid1 m
            eid2 = sleepMinuteMax m
            minute2 = maxMinute eid2 m
        in (eid1 * minute1, eid2 * minute2)

day04 :: String -> Showable
day04 = maybe (pack "bad input") (pack . go) . parseInput
