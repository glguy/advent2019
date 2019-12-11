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

import           Advent
import           Advent.Coord
import           Advent.Intcode
import           Data.Map (Map)
import qualified Data.Map as Map

main :: IO ()
main =
  do [inp] <- getParsedLines 11 memoryParser

     let effect = run (new inp)
         run1   = robot origin north effect Map.empty
         run2   = robot origin north effect (Map.singleton origin 1)
         render = putStrLn . draw . fmap render1

     render run1
     print (Map.size run1)
     render run2

render1 :: Integer -> Char
render1 1 = '█'
render1 0 = '░'
render1 _ = 'X'

robot :: Coord -> Coord -> Effect -> Map Coord Integer -> Map Coord Integer
robot here dir effect paint =
  case effect of
    Halt -> paint
    Input f -> robot here dir (f (Map.findWithDefault 0 here paint)) paint
    Output color (Output turn effect') -> robot here' dir' effect' paint'
      where
        paint' = Map.insert here color paint
        dir'   = turnFn turn dir
        here'  = addCoord here dir'
    _ -> error "bad robot"

turnFn :: Integer -> Coord -> Coord
turnFn 0 = turnLeft
turnFn 1 = turnRight
turnFn x = error ("Unexpected turn: " ++ show x)

draw :: Map Coord Char -> String
draw pixels = unlines [ [ pixel (C y x) | x <- [minx .. maxx]] | y <- [miny .. maxy] ]
  where
    pixel c = Map.findWithDefault ' ' c pixels
    Just (C miny minx, C maxy maxx) = boundingBox (Map.keys pixels)
