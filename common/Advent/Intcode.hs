{-# Language RecordWildCards, OverloadedStrings #-}
{-|
Module      : Advent.Intcode
Description : Intcode interpreter
Copyright   : (c) Eric Mertens, 2019
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2019/day/2>
<https://adventofcode.com/2019/day/5>
<https://adventofcode.com/2019/day/7>

This task expands the virtual machine defined in day 2
adding jumps, conditionals, inputs, and outputs.

This solution works with the following passes:

  1. Parse input text file into a list of numbers
  2. Execute op codes to extract the input/output "effects"
  3. Evaluate the effect as a function from a list of inputs to list of outputs
  4. Apply the function to a single input and find the last output.

>>> let check = effectList . run 0 . new

>>> check [3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9] <$> [[0],[10]]
[[0],[1]]

>>> check [3,3,1105,-1,9,1101,0,0,12,4,12,99,1] <$> [[0],[10]]
[[0],[1]]

>>> :{
>>> check [3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,
>>>       1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,
>>>       999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99]
>>>   <$> [[7],[8],[9]]
>>> :}
[[999],[1000],[1001]]

-}
module Advent.Intcode where

import           Advent        (Parser, number, sepBy)
import           Data.Bool     (bool)
import           Data.Sequence (Seq)
import qualified Data.Sequence as Seq

intCodeToList :: [Int] -> [Int] -> [Int]
intCodeToList pgm = effectList (run 0 (new pgm))

type Memory = Seq Int

(!) :: Memory -> Int -> Int
(!) = Seq.index

new :: [Int] -> Memory
new = Seq.fromList

set :: Int -> Int -> Memory -> Memory
set i v m = v `seq` Seq.update i v m

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
run :: Int -> Memory -> Effect
run pc mem = result
  where
    -- Dereferenced opcode argument
    val (Imm i) = i
    val (Ptr p) = mem ! p

    sav (Imm _) = error "write to immediate"
    sav (Ptr p) = set p

    result =
      case decode mem pc of
        Add a b c -> run (pc + 4) (sav c (val a + val b) mem)
        Mul a b c -> run (pc + 4) (sav c (val a * val b) mem)
        Inp a     -> Input (\i -> run (pc + 2) (sav a i mem))
        Out a     -> Output (val a) (run (pc + 2) mem)
        Jnz a b   -> run (bool (val b) (pc + 3) (val a == 0)) mem
        Jez a b   -> run (bool (val b) (pc + 3) (val a /= 0)) mem
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


data Param
  = Imm Int
  | Ptr Int
  deriving (Eq, Ord, Read, Show)

data Opcode
  = Add Param Param Param
  | Mul Param Param Param
  | Inp Param
  | Out Param
  | Jnz Param Param
  | Jez Param Param
  | Lt  Param Param Param
  | Eq  Param Param Param
  | Hlt
  deriving (Eq, Ord, Read, Show)

decode :: Memory -> Int -> Opcode
decode mem pc =
  let
    -- Opcode argument
    arg i = mem ! (pc + i)

    -- Parameter mode
    mode i = digit (i+1) (arg 0)

    opcode = arg 0 `mod` 100

    par i =
      case mode i of
        0 -> Ptr (arg i)
        1 -> Imm (arg i)
        m -> error ("Bad parameter mode: " ++ show m)
  in
  case opcode of
    1  -> Add (par 1) (par 2) (par 3)
    2  -> Mul (par 1) (par 2) (par 3)
    3  -> Inp (par 1)
    4  -> Out (par 1)
    5  -> Jnz (par 1) (par 2)
    6  -> Jez (par 1) (par 2)
    7  -> Lt  (par 1) (par 2) (par 3)
    8  -> Eq  (par 1) (par 2) (par 3)
    99 -> Hlt
    o  -> error ("Bad opcode " ++ show o)
