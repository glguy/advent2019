{-# Language DeriveTraversable, BlockArguments, RecordWildCards, OverloadedStrings #-}
{-|
Module      : Advent.Intcode
Description : Intcode interpreter
Copyright   : (c) Eric Mertens, 2019
License     : ISC
Maintainer  : emertens@gmail.com

This Intcode interpreter is defined across multiple Advent of Code days:

* <https://adventofcode.com/2019/day/2>
* <https://adventofcode.com/2019/day/5>
* <https://adventofcode.com/2019/day/7>

This implementation works with the following passes:

  1. Parse input text file into a list of numbers
  2. Execute op codes to extract the input/output "effects"
  3. Evaluate the effect as a function from a list of inputs to list of outputs

>>> intCodeToList [3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9] <$> [[0],[10]]
[[0],[1]]

>>> intCodeToList [3,3,1105,-1,9,1101,0,0,12,4,12,99,1] <$> [[0],[10]]
[[0],[1]]

>>> :{
>>> intCodeToList
>>>   [3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,
>>>    1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,
>>>    999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99]
>>> <$> [[7],[8],[9]]
>>> :}
[[999],[1000],[1001]]

-}
module Advent.Intcode
  (
  -- * Simple interface
  memoryParser,
  intCodeToList,

  -- * Machine state
  Machine(..), (!), new, set, memoryList,

  -- * Effects
  Effect(..), run,

  -- * Small-step
  Step(..), step,

  -- * Opcodes
  Opcode(..), decode,
  ) where

import           Advent    (Parser, number, sepBy)
import           Data.Bool (bool)
import           Data.Map (Map)
import           Data.Traversable (mapAccumL)
import qualified Data.Map as Map
import           Text.Printf (printf)
import           Data.List (intercalate)

------------------------------------------------------------------------
-- High-level interface
------------------------------------------------------------------------

-- | Run a given memory image as a list transducer.
intCodeToList ::
  [Integer] {- ^ initial memory -} ->
  [Integer] {- ^ inputs         -} ->
  [Integer] {- ^ outputs        -}
intCodeToList pgm = effectList (run (new pgm))

-- | Evaluate a program's effect as a function from a list of
-- inputs to a list of outputs.
effectList :: Effect -> [Integer] {- ^ inputs -} -> [Integer] {- ^ outputs -}
effectList effect inputs =
  case effect of
    Input f | x:xs <- inputs -> effectList (f x) xs
            | otherwise      -> error "Not enough inputs"
    Output o e               -> o : effectList e inputs
    Halt                     -> []

------------------------------------------------------------------------
-- Machine state
------------------------------------------------------------------------

-- | Program memory
data Machine = Machine
  { pc      :: !Integer
  , relBase :: !Integer
  , memory  :: !(Map Integer Integer)
  }

-- | Index memory at 0-based index
(!) :: Machine -> Integer -> Integer
m ! i = Map.findWithDefault 0 i (memory m)

-- | Construct memory from a list of initial values.
new :: [Integer] -> Machine
new initialValues = Machine
  { pc      = 0
  , relBase = 0
  , memory  = Map.fromList (zip [0..] initialValues)
  }

-- | Update the value stored at a given location in memory.
set ::
  Integer {- ^ position  -} ->
  Integer {- ^ new value -} ->
  Machine -> Machine
set i 0 m = m { memory = Map.delete i   (memory m) }
set i v m = m { memory = Map.insert i v (memory m) }

adjustRelBase :: Integer -> Machine -> Machine
adjustRelBase i mach = mach { relBase = relBase mach + i }

jmp :: Integer -> Machine -> Machine
jmp i mach = mach { pc = i }

memoryList :: Machine -> [Integer]
memoryList mach
  | Map.null (memory mach) = []
  | otherwise = map (mach !) [0 .. fst (Map.findMax (memory mach))]

------------------------------------------------------------------------
-- Parsing
------------------------------------------------------------------------

-- | Parse an Intcode program as a list of comma separated opcode integers.
memoryParser :: Parser [Integer]
memoryParser = number `sepBy` ","

------------------------------------------------------------------------
-- Big-step semantics
------------------------------------------------------------------------

-- | Possible effects from running a machine
data Effect
  = Output !Integer Effect     -- ^ Output an integer
  | Input (Integer -> Effect) -- ^ Input an integer
  | Halt                  -- ^ Halt execution

-- | Big-step semantics of virtual machine.
run :: Machine -> Effect
run mach =
  case step mach of
    Step mach'        -> run mach'
    StepOut out mach' -> Output out (run mach')
    StepIn f          -> Input (run . f)
    StepHalt _        -> Halt

------------------------------------------------------------------------
-- Small-step semantics
------------------------------------------------------------------------

-- | Result of small-step semantics.
data Step
  = Step    !Machine             -- ^ no effect
  | StepOut !Integer !Machine    -- ^ output
  | StepIn  (Integer -> Machine) -- ^ input
  | StepHalt !Machine            -- ^ halt

-- | Small-step semantics of virtual machine.
step :: Machine -> Step
step mach =
  case mapWithIndex toPtr (pc mach + 1) $! opcodeMode of
    (pc', opcode) -> impl opcode $! jmp pc' mach

  where
    at :: Integer -> Integer
    at i = mach ! i

    toPtr :: Integer -> Mode -> Integer
    toPtr i Imm = i
    toPtr i Abs = at i
    toPtr i Rel = at i + relBase mach

    opcodeMode :: Opcode Mode
    opcodeMode = decode (at (pc mach))

    impl opcode =
      case opcode of
        Add a b c           -> Step . set c (at a + at b)
        Mul a b c           -> Step . set c (at a * at b)
        Inp a               -> StepIn . flip (set a)
        Out a               -> StepOut (at a)
        Jnz a b | at a == 0 -> Step
                | otherwise -> Step . jmp (at b)
        Jz  a b | at a /= 0 -> Step
                | otherwise -> Step . jmp (at b)
        Lt  a b c           -> Step . set c (bool 0 1 (at a <  at b))
        Eq  a b c           -> Step . set c (bool 0 1 (at a == at b))
        Arb a               -> Step . adjustRelBase (at a)
        Hlt                 -> StepHalt

mapWithIndex :: (Integer -> a -> b) -> Integer -> Opcode a -> (Integer, Opcode b)
mapWithIndex f = mapAccumL (\i a -> (i+1, f i a))
{-# INLINE mapWithIndex #-}

------------------------------------------------------------------------
-- Opcode decoder
------------------------------------------------------------------------

-- | Parameter modes
data Mode
  = Abs -- ^ absolute position
  | Imm -- ^ immediate
  | Rel -- ^ relative position
  deriving (Eq, Ord, Read, Show)

-- | Opcodes parameterized over argument representations.
data Opcode a
  = Add !a !a !a -- ^ addition:        @c = a + b@
  | Mul !a !a !a -- ^ multiplication:  @c = a * b@
  | Inp !a       -- ^ input:           @a = input()@
  | Out !a       -- ^ output:          @output(a)@
  | Jnz !a !a    -- ^ jump-if-true:    @if a then goto b@
  | Jz  !a !a    -- ^ jump-if-false:   @if !a then goto b@
  | Lt  !a !a !a -- ^ less-than:       @c = a < b@
  | Eq  !a !a !a -- ^ equals:          @c = a == b@
  | Arb !a       -- ^ adjust-rel-base: @rel += a@
  | Hlt          -- ^ halt
  deriving (Eq, Ord, Read, Show, Functor, Foldable)

-- | Decode an intruction
--
-- >>> decode 1002
-- Mul Abs Imm Abs
decode :: Integer -> Opcode Mode
decode n = par <$> opcode
  where
    par i =
      case digit (i+1) n of
        0 -> Abs
        1 -> Imm
        2 -> Rel
        m -> error ("Bad parameter mode: " ++ show m)

    opcode =
      case n `mod` 100 of
        1  -> Add 1 2 3
        2  -> Mul 1 2 3
        3  -> Inp 1
        4  -> Out 1
        5  -> Jnz 1 2
        6  -> Jz  1 2
        7  -> Lt  1 2 3
        8  -> Eq  1 2 3
        9  -> Arb 1
        99 -> Hlt
        o  -> error ("Bad opcode " ++ show o)

instance Traversable Opcode where
  {-# INLINE traverse #-}
  traverse f o =
    case o of
      Add x y z -> Add <$> f x <*> f y <*> f z
      Mul x y z -> Mul <$> f x <*> f y <*> f z
      Inp x     -> Inp <$> f x
      Out x     -> Out <$> f x
      Jnz x y   -> Jnz <$> f x <*> f y
      Jz  x y   -> Jz  <$> f x <*> f y
      Lt  x y z -> Lt  <$> f x <*> f y <*> f z
      Eq  x y z -> Eq  <$> f x <*> f y <*> f z
      Arb x     -> Arb <$> f x
      Hlt       -> pure Hlt

-- | Extract the ith digit from a number.
--
-- >>> digit 0 2468
-- 8
-- >>> digit 3 2468
-- 2
-- >>> digit 4 2468
-- 0
digit :: Integer {- ^ position -} -> Integer {- ^ number -} -> Integer {- ^ digit -}
digit i x = x `div` (10^i) `mod` 10
