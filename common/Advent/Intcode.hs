{-# Language RecordWildCards, OverloadedStrings #-}
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

  -- * Memory
  Memory, (!), new, set,

  -- * Effects
  Effect(..), run,

  -- * Opcodes
  Opcode(..), Param(..), decode,
  ) where

import           Advent        (Parser, number, sepBy)
import           Data.Bool     (bool)
import           Data.Sequence (Seq)
import qualified Data.Sequence as Seq

-- | Run a given memory image as a list transducer.
intCodeToList ::
  [Int] {- ^ initial memory -} ->
  [Int] {- ^ inputs         -} ->
  [Int] {- ^ outputs        -}
intCodeToList pgm = effectList (run 0 (new pgm))

-- | Program memory
type Memory = Seq Int

-- | Index memory at 0-based index
(!) :: Memory -> Int -> Int
(!) = Seq.index

-- | Construct memory from a list of initial values.
new :: [Int] -> Memory
new = Seq.fromList

-- | Update the value stored at a given location in memory.
set ::
  Int {- ^ position  -} ->
  Int {- ^ new value -} ->
  Memory -> Memory
set i v m = v `seq` Seq.update i v m

-- | Parse an Intcode program as a list of comma separated opcode integers.
memoryParser :: Parser [Int]
memoryParser = number `sepBy` ","

-- | Evaluate a program's effect as a function from a list of
-- inputs to a list of outputs.
effectList :: Effect -> [Int] {- ^ inputs -} -> [Int] {- ^ outputs -}
effectList effect inputs =
  case effect of
    Input f | x:xs <- inputs -> effectList (f x) xs
            | otherwise      -> error "Not enough inputs"
    Output o e               -> o : effectList e inputs
    Halt                     -> []

-- | Possible effects from running a machine
data Effect
  = Output Int Effect     -- ^ Output an integer
  | Input (Int -> Effect) -- ^ Input an integer
  | Halt                  -- ^ Halt execution

-- | Compute the effect of running a machine.
run :: Int {- ^ program counter -} -> Memory -> Effect
run pc mem = result
  where
    -- Dereferenced opcode argument
    val (Imm i) = i
    val (Pos p) = mem ! p

    sav (Imm _) = error "write to immediate"
    sav (Pos p) = set p

    result =
      case decode mem pc of
        Add a b c -> run (pc + 4) (sav c (val a + val b) mem)
        Mul a b c -> run (pc + 4) (sav c (val a * val b) mem)
        Inp a     -> Input (\i -> run (pc + 2) (sav a i mem))
        Out a     -> Output (val a) (run (pc + 2) mem)
        Jnz a b   -> run (bool (val b) (pc + 3) (val a == 0)) mem
        Jz  a b   -> run (bool (val b) (pc + 3) (val a /= 0)) mem
        Lt  a b c -> run (pc + 4) (sav c (bool 0 1 (val a <  val b)) mem)
        Eq  a b c -> run (pc + 4) (sav c (bool 0 1 (val a == val b)) mem)
        Hlt       -> Halt

-- | Extract the ith digit from a number.
--
-- >>> digit 0 2468
-- 8
-- >>> digit 3 2468
-- 2
-- >>> digit 4 2468
-- 0
digit :: Int {- ^ position -} -> Int {- ^ number -} -> Int {- ^ digit -}
digit i x = x `div` (10^i) `mod` 10

-- | Opcode parameters
data Param
  = Imm Int -- ^ Immediate parameter
  | Pos Int -- ^ Position parameter
  deriving (Eq, Ord, Read, Show)

-- | VM opcodes
data Opcode
  = Add Param Param Param -- ^ addition:       @c = a + b@
  | Mul Param Param Param -- ^ multiplication: @c = a * b@
  | Inp Param             -- ^ input:          @a = input()@
  | Out Param             -- ^ output:         @output(a)@
  | Jnz Param Param       -- ^ jump-if-true:   @if a then goto b@
  | Jz  Param Param       -- ^ jump-if-false:  @if !a then goto b@
  | Lt  Param Param Param -- ^ less-than:      @c = a < b@
  | Eq  Param Param Param -- ^ equals:         @c = a == b@
  | Hlt                   -- ^ halt
  deriving (Eq, Ord, Read, Show)

-- | Decode an intruction
--
-- >>> decode (new [1002,4,3,4]) 0
-- Mul (Pos 4) (Imm 3) (Pos 4)
decode :: Memory -> Int {- ^ program counter -} -> Opcode
decode mem pc =
  let
    -- Opcode argument
    arg i = mem ! (pc + i)

    -- Parameter mode
    mode i = digit (i+1) (arg 0)

    opcode = arg 0 `mod` 100

    par i =
      case mode i of
        0 -> Pos (arg i)
        1 -> Imm (arg i)
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
    99 -> Hlt
    o  -> error ("Bad opcode " ++ show o)
