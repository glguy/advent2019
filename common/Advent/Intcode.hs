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
import qualified Data.Map as Map

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

adv :: Integer -> Machine -> Machine
adv i mach = mach { pc = pc mach + i }

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
  = Step    !Machine          -- ^ pc, memory
  | StepOut !Integer !Machine      -- ^ output, pc, memory
  | StepIn  (Integer -> Machine) -- ^ input -> (pc, memory)
  | StepHalt !Machine         -- ^ halt

-- | Small-step semantics of virtual machine.
step :: Machine -> Step
step mach = result mach
  where
    -- Dereferenced opcode argument
    val i = mach ! i

    result =
      case decode mach of
        Add a b c -> Step . adv 4 . set c (val a + val b)
        Mul a b c -> Step . adv 4 . set c (val a * val b)
        Inp a     -> \m -> StepIn (\i -> adv 2 (set a i m))
        Out a     -> StepOut (val a) . adv 2
        Jnz a b   | val a == 0 -> Step . adv 3
                  | otherwise  -> Step . jmp (val b)
        Jz  a b   | val a /= 0 -> Step . adv 3
                  | otherwise  -> Step . jmp (val b)
        Lt  a b c -> Step . adv 4 . set c (bool 0 1 (val a <  val b))
        Eq  a b c -> Step . adv 4 . set c (bool 0 1 (val a == val b))
        Arb a     -> Step . adv 2 . adjustRelBase (val a)
        Hlt       -> StepHalt . adv 1

------------------------------------------------------------------------
-- Opcode decoder
------------------------------------------------------------------------

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

-- | VM opcodes. Each parameter is resolved to a pointer.
data Opcode
  = Add !Integer !Integer !Integer -- ^ addition:        @c = a + b@
  | Mul !Integer !Integer !Integer -- ^ multiplication:  @c = a * b@
  | Inp !Integer                   -- ^ input:           @a = input()@
  | Out !Integer                   -- ^ output:          @output(a)@
  | Jnz !Integer !Integer          -- ^ jump-if-true:    @if a then goto b@
  | Jz  !Integer !Integer          -- ^ jump-if-false:   @if !a then goto b@
  | Lt  !Integer !Integer !Integer -- ^ less-than:       @c = a < b@
  | Eq  !Integer !Integer !Integer -- ^ equals:          @c = a == b@
  | Arb !Integer                   -- ^ adjust-rel-base: @rel += a@
  | Hlt                            -- ^ halt
  deriving (Eq, Ord, Read, Show)

-- | Decode an intruction
--
-- >>> decode (new [1002,4,3,4])
-- Mul 4 2 4
decode :: Machine -> Opcode
decode mach =
  let
    -- Opcode argument
    n = mach ! (pc mach)

    -- Parameter mode
    mode i = digit (i+1) n

    opcode = n `mod` 100

    par i =
      let a = i + pc mach in
      case mode i of
        0 -> mach ! a                -- position
        1 ->        a                -- immediate
        2 -> mach ! a + relBase mach -- relative
        m -> error ("Bad parameter mode: " ++ show m)
  in
  case opcode of
    1  -> Add (par 1) (par 2) (par 3)
    2  -> Mul (par 1) (par 2) (par 3)
    3  -> Inp (par 1)
    4  -> Out (par 1)
    5  -> Jnz (par 1) (par 2)
    6  -> Jz  (par 1) (par 2)
    7  -> Lt  (par 1) (par 2) (par 3)
    8  -> Eq  (par 1) (par 2) (par 3)
    9  -> Arb (par 1)
    99 -> Hlt
    o  -> error ("Bad opcode " ++ show o)
