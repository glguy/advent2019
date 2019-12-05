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
     let go i = print $ head $ outs $ run $ newMachine [i] pgm
     go 1
     go 5

-- | Machine state
data Machine = Machine
  { pc   :: Int    -- ^ program counter
  , mem  :: Memory -- ^ memory
  , ins  :: [Int]  -- ^ available inputs
  , outs :: [Int]  -- ^ outputs (most recent first)
  }

-- | Generate a fresh machine state starting at program counter @0@
-- with no outputs.
newMachine :: [Int] {- ^ inputs -} -> Memory {- ^ initial memory -} -> Machine
newMachine ins mem = Machine { pc = 0, mem = mem, ins = ins, outs = [] }

-- | Step a machine until it halts.
run :: Machine -> Machine
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

        3  -> run m{ pc = pc + 2, mem = set (arg 1) (head ins) mem, ins = tail ins  }
        4  -> run m{ pc = pc + 2, outs = val 1 : outs                               }

        5  -> run m{ pc = bool (val 2) (pc + 3) (val 1 == 0)                        }
        6  -> run m{ pc = bool (val 2) (pc + 3) (val 1 /= 0)                        }

        7  -> run m{ pc = pc + 4, mem = set (arg 3) (bool 0 1 (val 1 <  val 2)) mem }
        8  -> run m{ pc = pc + 4, mem = set (arg 3) (bool 0 1 (val 1 == val 2)) mem }

        99 -> m
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
