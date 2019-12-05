{-# Language RecordWildCards, OverloadedStrings #-}
{-|
Module      : Main
Description : Day 5 solution
Copyright   : (c) Eric Mertens, 2019
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2019/day/5>

>>> let parse = head . either error id . Advent.parseLines memoryParser

>>> let pgm = parse "3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9"
>>> outs (run (newMachine [0] pgm))
[0]
>>> outs (run (newMachine [10] pgm))
[1]

>>> let pgm = parse "3,3,1105,-1,9,1101,0,0,12,4,12,99,1"
>>> outs (run (newMachine [0] pgm))
[0]
>>> outs (run (newMachine [10] pgm))
[1]

>>> :{
>>> let pgm = parse "3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,\
>>>                 \1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,\
>>>                 \999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99"
>>> :}

>>> outs (run (newMachine [7] pgm))
[999]
>>> outs (run (newMachine [8] pgm))
[1000]
>>> outs (run (newMachine [9] pgm))
[1001]

-}
module Main (main) where

import           Advent        (Parser, getParsedLines, number, sepBy)
import           Data.Bool     (bool)
import           Data.Sequence (Seq)
import qualified Data.Sequence as Seq

type Memory = Seq Int

(!) :: Memory -> Int -> Int
(!) = Seq.index

new :: [Int] -> Memory
new = Seq.fromList

set :: Int -> Int -> Memory -> Memory
set i v m = v `seq` Seq.update i v m

memoryParser :: Parser Memory
memoryParser = new <$> number `sepBy` ","

main :: IO ()
main =
  do [pgm] <- getParsedLines 5 memoryParser
     let go i = print $ driver i (error "no output") $ run $ newMachine pgm
     go 1
     go 5

-- | Evaluate a program's effect given a constant input. Remember the
-- last output and return it.
driver :: Int {- ^ input -} -> Int {- ^ last output -} -> Effect -> Int
driver input output effect =
  case effect of
    Halt       -> output
    Input f    -> driver input output (f input)
    Output o e -> driver input o e

-- | Machine state
data Machine = Machine
  { pc   :: !Int   -- ^ program counter
  , mem  :: Memory -- ^ memory
  }

-- | Possible effects from running a machine
data Effect
  = Output Int Effect     -- | Output an integer
  | Input (Int -> Effect) -- | Input an integer
  | Halt                  -- | Halt execution

-- | Generate a fresh machine state starting at program counter @0@
-- with no outputs.
newMachine :: Memory {- ^ initial memory -} -> Machine
newMachine mem = Machine { pc = 0, mem = mem }

-- | Compute the effect of running a machine.
run :: Machine -> Effect
run m@Machine{..} = result
  where
    -- Opcode argument
    arg i = mem ! (pc + i)

    -- Dereferenced opcode argument
    val i =
      case mode i of
        0 -> mem ! arg i
        1 -> arg i
        x -> error ("bad parameter mode " ++ show x ++ " at " ++ show pc)

    -- Parameter mode
    mode i = digit (i+1) (arg 0)

    opcode = arg 0 `mod` 100

    result =
      case opcode of
        1  -> run m{ pc = pc + 4, mem = set (arg 3) (val 1 + val 2) mem             }
        2  -> run m{ pc = pc + 4, mem = set (arg 3) (val 1 * val 2) mem             }

        3  -> Input (\i -> run m{ pc = pc + 2, mem = set (arg 1) i mem })
        4  -> Output (val 1) (run m{ pc = pc + 2 })

        5  -> run m{ pc = bool (val 2) (pc + 3) (val 1 == 0)                        }
        6  -> run m{ pc = bool (val 2) (pc + 3) (val 1 /= 0)                        }

        7  -> run m{ pc = pc + 4, mem = set (arg 3) (bool 0 1 (val 1 <  val 2)) mem }
        8  -> run m{ pc = pc + 4, mem = set (arg 3) (bool 0 1 (val 1 == val 2)) mem }

        99 -> Halt
        o  -> error ("Bad opcode " ++ show o ++ " at " ++ show pc)

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
