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

import Advent         (getParsedLines)
import Advent.Intcode (intCodeToList, memoryParser)

main :: IO ()
main =
  do [inp] <- getParsedLines 9 memoryParser
     let go i = print (head (intCodeToList inp [i]))
     go 1
     go 2
