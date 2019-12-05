{-# Language OverloadedStrings #-}
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
     print (head (programTrace [1] [] 0 pgm))
     print (head (programTrace [5] [] 0 pgm))

programTrace :: [Int] -> [Int] -> Int {- ^ program counter -} -> Memory -> [Int]
programTrace ins outs pc pgm = result
  where
    -- Opcode argument
    arg i = pgm ! (pc + i)

    -- Dereferenced opcode argument
    val i
      | digit (i+1) (arg 0) == 0 = pgm ! arg i
      | digit (i+1) (arg 0) == 1 = arg i

    result =
      case arg 0 `mod` 100 of
        a | a >= 10000 -> error (show a) -- sanity check
        1                  -> programTrace ins outs (pc + 4) (set (arg 3) (val 1 + val 2) pgm)
        2                  -> programTrace ins outs (pc + 4) (set (arg 3) (val 1 * val 2) pgm)
        3                  -> programTrace (tail ins) outs (pc + 2) (set (arg 1) (head ins) pgm)
        4                  -> programTrace ins (val 1 : outs) (pc + 2) pgm
        5  | val 1 == 0    -> programTrace ins outs (pc + 3) pgm
           | otherwise     -> programTrace ins outs (val 2) pgm
        6  | val 1 /= 0    -> programTrace ins outs (pc + 3) pgm
           | otherwise     -> programTrace ins outs (val 2) pgm
        7  | val 1 < val 2 -> programTrace ins outs (pc + 4) (set (arg 3) 1 pgm)
           | otherwise     -> programTrace ins outs (pc + 4) (set (arg 3) 0 pgm)
        8  | val 1 ==val 2 -> programTrace ins outs (pc + 4) (set (arg 3) 1 pgm)
           | otherwise     -> programTrace ins outs (pc + 4) (set (arg 3) 0 pgm)
        99 -> outs
        o  -> error ("Bad opcode " ++ show o ++ " at " ++ show pc)

digit :: Int -> Int -> Int
digit i x = x `div` (10^i) `mod` 10
