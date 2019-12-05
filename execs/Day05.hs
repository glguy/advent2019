{-# Language RecordWildCards, OverloadedStrings #-}
{-|
Module      : Main
Description : Day 5 solution
Copyright   : (c) Eric Mertens, 2019
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2019/day/5>

-}
module Main (main) where

import           Advent (Parser, getParsedLines, number, sepBy)
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
     let mach i = Machine { pc = 0, mem = pgm, ins = [i], outs = [] }
     print $ head $ outs $ run $ mach 1
     print $ head $ outs $ run $ mach 5

data Machine = Machine
  { pc :: Int
  , mem :: Memory
  , ins :: [Int]
  , outs :: [Int]
  }

run :: Machine -> Machine
run m@Machine{..} = result
  where
    -- Opcode argument
    arg i = mem ! (pc + i)

    -- Dereferenced opcode argument
    val i =
      case digit (i+1) (arg 0) of
        0 -> mem ! arg i
        1 -> arg i
        x -> error ("bad parameter mode: " ++ show x)

    result =
      case arg 0 `mod` 100 of
        a | a >= 10000 -> error (show a) -- sanity check
        1                  -> run m{ pc = pc + 4, mem = set (arg 3) (val 1 + val 2) mem }
        2                  -> run m{ pc = pc + 4, mem = set (arg 3) (val 1 * val 2) mem }
        3                  -> run m{ pc = pc + 2, mem = set (arg 1) (head ins) mem, ins = tail ins }
        4                  -> run m{ pc = pc + 2, outs = val 1 : outs }
        5  | val 1 == 0    -> run m{ pc = pc + 3 }
           | otherwise     -> run m{ pc = val 2 }
        6  | val 1 /= 0    -> run m{ pc = pc + 3 }
           | otherwise     -> run m{ pc = val 2 }
        7  | val 1 < val 2 -> run m{ pc = pc + 4, mem = set (arg 3) 1 mem }
           | otherwise     -> run m{ pc = pc + 4, mem = set (arg 3) 0 mem }
        8  | val 1 ==val 2 -> run m{ pc = pc + 4, mem = set (arg 3) 1 mem }
           | otherwise     -> run m{ pc = pc + 4, mem = set (arg 3) 0 mem }
        99 -> m
        o  -> error ("Bad opcode " ++ show o ++ " at " ++ show pc)

digit :: Int -> Int -> Int
digit i x = x `div` (10^i) `mod` 10
