module Day19 (day19) where

import Control.Applicative hiding (many)
import Data.Bits
import Data.Map.Lazy (Map, (!))
import qualified Data.Map.Lazy as Map

import Types
import Parser

type OpFn = Int -> Int -> Int -> Registers -> Registers

data Op = ADDR | ADDI | MULR | MULI | BANR | BANI | BORR | BORI | SETR | SETI
        | GTIR | GTRI | GTRR | EQIR | EQRI | EQRR
  deriving (Eq, Ord, Show)

data OpInput = OpInput Int Int Int
  deriving Show

data Instruction = Instruction Op OpInput
  deriving Show

data Registers = Registers Int [Int]
  deriving Show

type IP = Int

data Program = Program Registers IP [Instruction]
  deriving Show

makeRegisters :: Int -> Registers
makeRegisters n = Registers n (replicate n 0)

makeRegisters' :: Int -> Registers
makeRegisters' n = Registers n (1 : (replicate (n-1) 0))

getRegister :: Int -> Registers -> Maybe Int
getRegister m (Registers n rs)
  | m >= n    = Nothing
  | m < 0     = Nothing
  | otherwise = Just $ rs !! m

setRegister :: Int -> Registers -> Int -> Registers
setRegister m rss@(Registers n rs) a
  | m >= n    = rss
  | m < 0     = rss
  | otherwise = let (xs, _:ys) = splitAt m rs
                in Registers n (xs ++ (a:ys))

getRegisterVals :: Registers -> [Int]
getRegisterVals (Registers _ rs) = rs

insr :: (Int -> Int -> Int) -> OpFn
insr op a b c r =
  case (getRegister a r, getRegister b r) of
    (Nothing, _) -> r
    (_, Nothing) -> r
    (Just a', Just b') -> setRegister c r $ op a' b'

insi :: (Int -> Int -> Int) -> OpFn
insi op a b c r =
  case (getRegister a r) of
    Nothing -> r
    Just a' -> setRegister c r $ op a' b

compir :: (Int -> Int -> Bool) -> OpFn
compir op a b c r =
  case (getRegister b r) of
    Nothing -> r
    Just b' -> if op a b'
               then setRegister c r 1
               else setRegister c r 0

compri :: (Int -> Int -> Bool) -> OpFn
compri op a b c r = 
  case (getRegister a r) of
    Nothing -> r
    Just a' -> if op a' b
               then setRegister c r 1
               else setRegister c r 0

comprr :: (Int -> Int -> Bool) -> OpFn
comprr op a b c r = 
  case (getRegister a r, getRegister b r) of
    (Nothing, _) -> r
    (_, Nothing) -> r
    (Just a', Just b') -> if op a' b'
                          then setRegister c r 1
                          else setRegister c r 0

ops :: Map Op OpFn
ops = Map.fromList [ (ADDR, insr (+))
                   , (ADDI, insi (+))
                   , (MULR, insr (*))
                   , (MULI, insi (*))
                   , (BANR, insr (.&.))
                   , (BANI, insi (.&.))
                   , (BORR, insr (.|.))
                   , (BORI, insi (.|.))
                   , (SETR, \a _ c r -> maybe r (setRegister c r) (getRegister a r))
                   , (SETI, \a _ c r -> setRegister c r a)
                   , (GTIR, compir (>))
                   , (GTRI, compri (>))
                   , (GTRR, comprr (>))
                   , (EQIR, compir (==))
                   , (EQRI, compri (==))
                   , (EQRR, comprr (==))
                   ]

instructionPointerParser :: Parser Int
instructionPointerParser = string "#ip " *> number

opInputParser :: Parser OpInput
opInputParser = OpInput <$> (number <* space) <*> (number <* space) <*> number

opParser :: Parser Op
opParser = f ADDR "addr" <|> f ADDI "addi" <|> f MULR "mulr" <|> f MULI "muli"
       <|> f BANR "banr" <|> f BANI "bani" <|> f BORR "borr" <|> f BORI "bori"
       <|> f SETR "setr" <|> f SETI "seti" <|> f GTIR "gtir" <|> f GTRI "gtri"
       <|> f GTRR "gtrr" <|> f EQIR "eqir" <|> f EQRI "eqri" <|> f EQRR "eqrr"
  where f o s = const o <$> (string s <* spaces)

instructionParser :: Parser Instruction
instructionParser = Instruction <$> opParser <*> opInputParser

programParser :: Parser (Program, Program)
programParser = makePrograms
            <$> instructionPointerParser
            <*> many instructionParser
  where makePrograms ip is = ( Program (makeRegisters 6) ip is
                             , Program (makeRegisters' 6) ip is
                             )

idxApply :: Int -> (a -> a) -> [a] -> [a]
idxApply i f as
  | i < 0 = as
  | i >= length as = as
  | otherwise = let (xs, y:ys) = splitAt i as
                in xs ++ (f y : ys)

execute :: Program -> Program
execute p@(Program rss@(Registers n rs) ip is)
  | ip < 0                = p
  | ip >= length rs       = p
  | rs !! ip < 0          = p
  | rs !! ip >= length is = p
  | otherwise             =
      let ip' = rs !! ip
          (Instruction op (OpInput a b c)) = is !! ip'
          rs' = idxApply ip succ $ getRegisterVals $ (ops ! op) a b c rss
      in execute $ Program (Registers n rs') ip is

programResult :: Program -> Int
programResult p = let (Program r _ _) = execute p
                  in maybe 0 id $ getRegister 0 r

go :: (Program, Program) -> (Int, Int)
-- go (p, q) = (programResult p, programResult q)
go (p, _) = (programResult p, 0)

day19 :: String -> Showable
day19 s = maybe (pack "Failed to parse input")
                (pack . go . fst)
                (runParser programParser (filter (/= '\n') s))
