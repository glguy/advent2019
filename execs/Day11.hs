{-# Language OverloadedStrings #-}
{-|
Module      : Main
Description : Day 11 solution
Copyright   : (c) Eric Mertens, 2019
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2019/day/11>

-}
module Main (main) where

import           Advent (getParsedLines)
import           Advent.Coord
import           Advent.Intcode (Effect(..), run, new, memoryParser)
import           Data.Map (Map)
import qualified Data.Map as Map

main :: IO ()
main =
  do [inp] <- getParsedLines 11 memoryParser

     let start  = robot origin north (run (new inp))
         run1   = start Map.empty
         run2   = start (Map.singleton origin 1)
         render = putStrLn . drawCoords . fmap paintChar

     render run1
     print (Map.size run1)
     render run2

-- | Run a painter robot to see what it paints.
robot ::
  Coord             {- ^ robot's location             -} ->
  Coord             {- ^ robot's direction            -} ->
  Effect            {- ^ control program effect       -} ->
  Map Coord Integer {- ^ starting painted coordinates -} ->
  Map Coord Integer {- ^ final painted coordinates    -}
robot here dir effect paint =
  case effect of

    Halt -> paint

    Input f -> robot here dir effect' paint
      where
        color   = Map.findWithDefault 0 here paint
        effect' = f color

    Output color (Output turn effect') -> robot here' dir' effect' paint'
      where
        paint' = Map.insert here color paint
        dir'   = turnFn turn dir
        here'  = addCoord here dir'

    _ -> error "Bad program"

-- | Compute the turn function given a robot's output.
turnFn :: Integer {- ^ robot turn output -} -> Coord -> Coord
turnFn 0 = turnLeft
turnFn 1 = turnRight
turnFn x = error ("Unexpected turn command: " ++ show x)

-- | Character representation of paint number.
paintChar :: Integer -> Char
paintChar 0 = '░'
paintChar 1 = '█'
paintChar x = error ("Unexpected paint color: " ++ show x)
