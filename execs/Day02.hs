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

import           Advent (getParsedLines, number, sepBy)
import           Data.Sequence (Seq)
import qualified Data.Sequence as Seq

type Memory = Seq Int

(!) :: Memory -> Int -> Int
(!) = Seq.index

new :: [Int] -> Memory
new = Seq.fromList

set :: Int -> Int -> Memory -> Memory
set i v m = v `seq` Seq.update i v m

main :: IO ()
main =
  do [pgm] <- getParsedLines 2 (new <$> number `sepBy` ",")
     print (startup 12 2 pgm)
     print (head [ 100 * noun + verb
                 | noun <- [0..99]
                 , verb <- [0..99]
                 , startup noun verb pgm == 19690720 ])

-- | Run the given program after assigning the given noun and verb.
startup :: Int {- ^ noun -} -> Int {- ^ verb -} -> Memory -> Int
startup noun verb
  = (! 0)
  . run 0
  . Seq.update 1 noun
  . Seq.update 2 verb

-- | Run the given program starting at the given program counter
-- returning the initial memory value once the program halts.
--
-- >>> let check = Data.Foldable.toList . run 0 . new
-- >>> check [1,0,0,0,99]
-- [2,0,0,0,99]
-- >>> check [2,3,0,3,99]
-- [2,3,0,6,99]
-- >>> check [2,4,4,5,99,0]
-- [2,4,4,5,99,9801]
-- >>> check [1,1,1,4,99,5,6,0,99]
-- [30,1,1,4,2,5,6,0,99]
-- >>> check [1,9,10,3,2,3,11,0,99,30,40,50]
-- [3500,9,10,70,2,3,11,0,99,30,40,50]
run :: Int -> Memory -> Memory
run i pgm = last (programTrace i pgm)

-- | Run a program providing a list of intermediate states of the program.
--
-- >>> let pgm = [1,9,10,3,2,3,11,0,99,30,40,50]
-- >>> mapM_ (print . Data.Foldable.toList) (programTrace 0 (new pgm))
-- [1,9,10,3,2,3,11,0,99,30,40,50]
-- [1,9,10,70,2,3,11,0,99,30,40,50]
-- [3500,9,10,70,2,3,11,0,99,30,40,50]
programTrace :: Int {- ^ program counter -} -> Memory -> [Memory]
programTrace pc pgm = pgm : rest
  where
    op2 (?) = programTrace (pc + 4) (set (arg 3) (val 1 ? val 2) pgm)

    -- Opcode argument
    arg i = pgm ! (pc + i)

    -- Dereferenced opcode argument
    val i = pgm ! arg i

    rest =
      case arg 0 of
        1  -> op2 (+)
        2  -> op2 (*)
        99 -> []
        o  -> error ("Bad opcode " ++ show o ++ " at " ++ show pc)
