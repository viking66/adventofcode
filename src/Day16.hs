-- module Day16 (day16) where
module Day16 where

import Data.Bits

import Types
import Parser

type Op = Int -> Int -> Int -> Registers -> Registers

data Instruction = Instruction Int Int Int Int
  deriving Show

data Registers = Registers Int Int Int Int
  deriving (Eq, Show)

data Sample = Sample Registers Instruction Registers
  deriving Show

registerParser :: Parser Registers
registerParser = Registers <$> (char '[' *> number)
                           <*> (string ", " *> number)
                           <*> (string ", " *> number)
                           <*> (string ", " *> number <* char ']')

instructionParser :: Parser Instruction
instructionParser = Instruction <$> number
                               <*> (char ' ' *> number)
                               <*> (char ' ' *> number)
                               <*> (char ' ' *> number)

sampleParser :: Parser Sample
sampleParser = Sample <$> (string "Before: " *> registerParser)
                      <*> (char '\n' *> instructionParser <* char '\n')
                      <*> (string "After:  " *> registerParser <* string "\n\n")

inputParser :: Parser ([Sample], [Instruction])
inputParser = ((,)) <$> (many sampleParser <* many (char '\n'))
                    <*> many (instructionParser <* char '\n')

parseInput :: String -> ([Sample], [Instruction])
parseInput = maybe ([], []) fst . runParser inputParser

getRegister :: Int -> Registers -> Int
getRegister 0 (Registers x _ _ _) = x
getRegister 1 (Registers _ x _ _) = x
getRegister 2 (Registers _ _ x _) = x
getRegister _ (Registers _ _ _ x) = x

setRegister :: Int -> Registers -> Int -> Registers
setRegister 0 (Registers _ x y z) a = Registers a x y z
setRegister 1 (Registers w _ y z) a = Registers w a y z
setRegister 2 (Registers w x _ z) a = Registers w x a z
setRegister _ (Registers w x y _) a = Registers w x y a

insr :: (Int -> Int -> Int) -> Op
insr op a b c r = setRegister c r $ op (getRegister a r) (getRegister b r)

insi :: (Int -> Int -> Int) -> Op
insi op a b c r = setRegister c r $ op (getRegister a r) b

compir :: (Int -> Int -> Bool) -> Op
compir op a b c r = if op a (getRegister b r)
                    then setRegister c r 1
                    else setRegister c r 0

compri :: (Int -> Int -> Bool) -> Op
compri op a b c r = if op (getRegister a r) b
                    then setRegister c r 1
                    else setRegister c r 0

comprr :: (Int -> Int -> Bool) -> Op
comprr op a b c r = if op (getRegister a r) (getRegister b r)
                    then setRegister c r 1
                    else setRegister c r 0

addr :: Op
addr = insr (+)

addi :: Op
addi = insi (+)

mulr :: Op
mulr = insr (*)

muli :: Op
muli = insi (*)

banr :: Op
banr = insr (.&.)

bani :: Op
bani = insi (.&.)

borr :: Op
borr = insr (.|.)

bori :: Op
bori = insi (.|.)

setr :: Op
setr a _ c r = setRegister c r (getRegister a r)

seti :: Op
seti a _ c r = setRegister c r a

gtir :: Op
gtir = compir (>)

gtri :: Op
gtri = compri (>)

gtrr :: Op
gtrr = comprr (>)

eqir :: Op
eqir = compir (==)

eqri :: Op
eqri = compri (==)

eqrr :: Op
eqrr = comprr (==)

ops :: [Op]
ops = [ addr, addi, mulr, muli, banr, bani, borr, bori
      , setr, seti, gtir, gtri, gtrr, eqir, eqri, eqrr
      ]

callOp :: Instruction -> Registers -> Op -> Registers
callOp (Instruction _ a b c) r op = op a b c r

possibleOp :: Sample -> Op -> Bool
possibleOp (Sample q i r) op = callOp i q op == r

go :: [Sample] -> Int
go = length . filter threeOrMore . map matchOps
  where matchOps x = filter (possibleOp x) ops
        threeOrMore = (>= 3) . length

day16 :: String -> Showable
day16 s = let (samples, _) = parseInput s
          in pack (go samples)
