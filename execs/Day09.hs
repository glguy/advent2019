{-# Language OverloadedStrings #-}
{-# Options_GHC -w #-}
{-|
Module      : Main
Description : Day 9 solution
Copyright   : (c) Eric Mertens, 2019
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2019/day/9>

>>> intCodeToList [109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99] []
[109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99]

>>> intCodeToList [1102,34915192,34915192,7,4,7,99,0] []
[1219070632396864]

>>> intCodeToList [104,1125899906842624,99] []
[1125899906842624]

-}
module Main (main) where

import           Advent
import           Advent.Intcode
import           Control.Applicative
import           Control.Monad
import           Data.List
import           Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import           Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Set (Set)
import qualified Data.Set as Set


main :: IO ()
main =
  do [inp] <- getParsedLines 9 memoryParser
     let pgm i = intCodeToList inp [i]
     print (pgm 1)
     print (pgm 2)
