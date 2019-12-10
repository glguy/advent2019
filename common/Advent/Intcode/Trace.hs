module Advent.Intcode.Trace where

import Advent
import Advent.Intcode
import Data.Foldable
import Data.List
import Text.Printf
import System.IO

traceRun :: Machine -> IO ()
traceRun mach =
  do printf "%8d: %s\n" (pc mach) (showCurrent mach)
     case step mach of
       Step mach' -> traceRun mach'
       StepIn f   ->
          do putStr "Input: "
             hFlush stdout
             i <- readLn
             traceRun (f i)
       StepOut o mach' ->
          do putStr "Output: "
             print o
             traceRun mach'
       StepHalt _ -> return ()

showCurrent :: Machine -> String
showCurrent mach =
  case decode (mach ! pc mach) of
    Nothing -> "<invalid>"
    Just o -> mnemonic o ++ concat (zipWith showArg [pc mach + 1..] (toList o))
  where
    showArg i Imm = ' ' : show (mach ! i)
    showArg i Abs = ' ' : '[' : show (mach ! i) ++ "]"
    showArg i Rel = ' ' : '<' : show (mach ! i) ++ ">"

showLocal :: Integer -> String
showLocal i =
  case decode i of
    Nothing -> "<invalid>"
    Just o -> mnemonic o ++ concatMap showArg o
  where
    showArg Imm = " imm"
    showArg Abs = " abs"
    showArg Rel = " rel"

showRow :: [Integer] -> [String]
showRow xs =
  [ intercalate " " [ printf "%16d" x | x <- xs]
  , intercalate " " [ printf "%16s" (showLocal x) | x <- xs]
  ]

showMachine :: Machine -> String
showMachine mach =
  unlines $
  concatMap showRow (chunks 8 (memoryList mach))

mnemonic :: Opcode a -> String
mnemonic o =
  case o of
    Add{} -> "add"
    Mul{} -> "add"
    Inp{} -> "in"
    Out{} -> "out"
    Jnz{} -> "jnz"
    Jz {} -> "jz"
    Lt {} -> "lt"
    Eq {} -> "eq"
    Arb{} -> "arb"
    Hlt{} -> "hlt"
