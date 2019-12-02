{-# Options_GHC -w #-}
{-# Language OverloadedStrings #-}
{-|
Module      : Main
Description : Day 2 solution
Copyright   : (c) Eric Mertens, 2019
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2019/day/2>

-}
module Main (main) where

import Advent
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

type Program = IntMap Int

(!) :: Program -> Int -> Int
(!) = (IntMap.!)

newProgram :: [Int] -> Program
newProgram = IntMap.fromList . zip [0..]

main :: IO ()
main =
  do inp <- getParsedInput 2 (number `sepBy` ",")
     let pgm = newProgram inp
     print (startup 12 2 pgm)
     print (head [ 100 * noun + verb
                 | noun <- [0..99]
                 , verb <- [0..99]
                 , startup noun verb pgm == 19690720 ])

-- | Run the given program after assigning the given noun and verb.
startup :: Int {- ^ noun -} -> Int {- ^ verb -} -> Program -> Int
startup noun verb
  = run 0
  . IntMap.insert 1 noun
  . IntMap.insert 2 verb

-- | Run the given program starting at the given program counter
-- returning the initial memory value once the program halts.
run :: Int {- ^ program counter -} -> Program -> Int
run i pgm =
  case arg 0 of
    1  -> math (+)
    2  -> math (*)
    99 -> pgm ! 0
  where
    math f = run (i+4) (IntMap.insert (arg 3) (val 1 `f` val 2) pgm)
    val j = pgm ! arg j
    arg j = pgm ! (i+j)
