module Day16 (day16) where

import Data.Bifunctor (second)
import Data.Bits
import qualified Data.List as List
import Data.Map.Lazy (Map, (!))
import qualified Data.Map.Lazy as Map
import Data.Set (Set)
import qualified Data.Set as Set

import Types
import Parser

type OpFn = Int -> Int -> Int -> Registers -> Registers

data Op = ADDR OpFn | ADDI OpFn | MULR OpFn | MULI OpFn | BANR OpFn | BANI OpFn
        | BORR OpFn | BORI OpFn | SETR OpFn | SETI OpFn | GTIR OpFn | GTRI OpFn
        | GTRR OpFn | EQIR OpFn | EQRI OpFn | EQRR OpFn

data Instruction = Instruction Int Int Int Int
  deriving Show

data Registers = Registers Int Int Int Int
  deriving (Eq, Show)

data Sample = Sample Registers Instruction Registers
  deriving Show

instance Eq Op where
  ADDR _ == ADDR _ = True
  ADDI _ == ADDI _ = True
  MULR _ == MULR _ = True
  MULI _ == MULI _ = True
  BANR _ == BANR _ = True
  BANI _ == BANI _ = True
  BORR _ == BORR _ = True
  BORI _ == BORI _ = True
  SETR _ == SETR _ = True
  SETI _ == SETI _ = True
  GTIR _ == GTIR _ = True
  GTRI _ == GTRI _ = True
  GTRR _ == GTRR _ = True
  EQIR _ == EQIR _ = True
  EQRI _ == EQRI _ = True
  EQRR _ == EQRR _ = True
  _ == _ = False

instance Show Op where
  show (ADDR _) = "ADDR"
  show (ADDI _) = "ADDI"
  show (MULR _) = "MULR"
  show (MULI _) = "MULI"
  show (BANR _) = "BANR"
  show (BANI _) = "BANI"
  show (BORR _) = "BORR"
  show (BORI _) = "BORI"
  show (SETR _) = "SETR"
  show (SETI _) = "SETI"
  show (GTIR _) = "GTIR"
  show (GTRI _) = "GTRI"
  show (GTRR _) = "GTRR"
  show (EQIR _) = "EQIR"
  show (EQRI _) = "EQRI"
  show (EQRR _) = "EQRR"

instance Ord Op where
  compare x y = compare (show x) (show y)

getOpFn :: Op -> OpFn
getOpFn (ADDR fn) = fn
getOpFn (ADDI fn) = fn
getOpFn (MULR fn) = fn
getOpFn (MULI fn) = fn
getOpFn (BANR fn) = fn
getOpFn (BANI fn) = fn
getOpFn (BORR fn) = fn
getOpFn (BORI fn) = fn
getOpFn (SETR fn) = fn
getOpFn (SETI fn) = fn
getOpFn (GTIR fn) = fn
getOpFn (GTRI fn) = fn
getOpFn (GTRR fn) = fn
getOpFn (EQIR fn) = fn
getOpFn (EQRI fn) = fn
getOpFn (EQRR fn) = fn

getInstructionId :: Instruction -> Int
getInstructionId (Instruction x _ _ _) = x

getSampleInstruction :: Sample -> Instruction
getSampleInstruction (Sample _ x _) = x

getSampleInstructionId :: Sample -> Int
getSampleInstructionId = getInstructionId . getSampleInstruction

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

insr :: (Int -> Int -> Int) -> OpFn
insr op a b c r = setRegister c r $ op (getRegister a r) (getRegister b r)

insi :: (Int -> Int -> Int) -> OpFn
insi op a b c r = setRegister c r $ op (getRegister a r) b

compir :: (Int -> Int -> Bool) -> OpFn
compir op a b c r = if op a (getRegister b r)
                    then setRegister c r 1
                    else setRegister c r 0

compri :: (Int -> Int -> Bool) -> OpFn
compri op a b c r = if op (getRegister a r) b
                    then setRegister c r 1
                    else setRegister c r 0

comprr :: (Int -> Int -> Bool) -> OpFn
comprr op a b c r = if op (getRegister a r) (getRegister b r)
                    then setRegister c r 1
                    else setRegister c r 0

addr :: OpFn
addr = insr (+)

addi :: OpFn
addi = insi (+)

mulr :: OpFn
mulr = insr (*)

muli :: OpFn
muli = insi (*)

banr :: OpFn
banr = insr (.&.)

bani :: OpFn
bani = insi (.&.)

borr :: OpFn
borr = insr (.|.)

bori :: OpFn
bori = insi (.|.)

setr :: OpFn
setr a _ c r = setRegister c r (getRegister a r)

seti :: OpFn
seti a _ c r = setRegister c r a

gtir :: OpFn
gtir = compir (>)

gtri :: OpFn
gtri = compri (>)

gtrr :: OpFn
gtrr = comprr (>)

eqir :: OpFn
eqir = compir (==)

eqri :: OpFn
eqri = compri (==)

eqrr :: OpFn
eqrr = comprr (==)

ops :: Set Op
ops = Set.fromList
  [ ADDR addr, ADDI addi, MULR mulr, MULI muli, BANR banr, BANI bani, BORR borr
  , BORI bori, SETR setr, SETI seti, GTIR gtir, GTRI gtri, GTRR gtrr, EQIR eqir
  , EQRI eqri, EQRR eqrr
  ]

callOpFn :: Instruction -> Registers -> OpFn -> Registers
callOpFn (Instruction _ a b c) r op = op a b c r

validOpFn :: Sample -> Op -> Bool
validOpFn (Sample q i r) op = callOpFn i q (getOpFn op) == r

getValidOpFn :: Sample -> (Int, Set Op)
getValidOpFn s = (getSampleInstructionId s, Set.filter (validOpFn s) ops)

determineOps :: Map Int (Set Op) -> Map Int Op
determineOps = normalize . simplifyOps [] . Map.toList
  where simplifyOps xs [] = xs
        simplifyOps xs ys = let (as, bs) = List.partition ((== 1) . Set.size . snd) ys
                                xs' = xs ++ as
                                s = foldr (\(_, a) -> Set.union a) Set.empty xs'
                                ys' = map (second (flip Set.difference s)) bs
                            in simplifyOps xs' ys'
        normalize = Map.fromList . map (second $ head . Set.toList)

go1 :: [Sample] -> Int
go1 = length . filter threeOrMore . map (snd . getValidOpFn)
  where threeOrMore = (>= 3) . length

go2 :: [Sample] -> [Instruction] -> Int
go2 ss is = let opMap = determineOps $ foldr f Map.empty $ map getValidOpFn ss
            in getRegister 0 $ foldl (g opMap) (Registers 0 0 0 0) is
  where f (i, o) = Map.insertWith Set.intersection i o
        g opMap r (Instruction o a b c) = getOpFn (opMap ! o) a b c r

day16 :: String -> Showable
day16 s = let (samples, instructions) = parseInput s
          in pack (go1 samples, go2 samples instructions)
