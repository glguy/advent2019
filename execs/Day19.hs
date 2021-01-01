{-|
Module      : Main
Description : Day 19 solution
Copyright   : (c) Eric Mertens, 2019
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2019/day/19>

-}
module Main (main) where

import Advent (getIntcodeInput)
import Intcode (intcodeToList)

main :: IO ()
main =
  do inp <- getIntcodeInput 19
     let p x y = 1 == head (intcodeToList inp [x,y])
     print (length [ () | x <- [0..49], y <- [0..49], p x y])
     print (part2 p 0 100)

part2 :: (Int -> Int -> Bool) -> Int -> Int -> Int
part2 p x y
  | bottomLeft, topRight = x * 10000 + y - 99
  | bottomLeft           = part2 p x (y+1)
  | otherwise            = part2 p (x+1) y
  where
    topRight   = p (x+99) (y-99)
    bottomLeft = p x y
