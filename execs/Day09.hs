{-# Language OverloadedStrings #-}
{-# Options_GHC -w #-}
{-|
Module      : Main
Description : Day 9 solution
Copyright   : (c) Eric Mertens, 2019
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2019/day/9>

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


