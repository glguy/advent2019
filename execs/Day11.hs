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
import           Advent.Intcode
import           Data.Map (Map)
import qualified Data.Map as Map

type C = (Int,Int)

main :: IO ()
main =
  do [inp] <- getParsedLines 11 memoryParser
     let effect = run (new inp)
     print $ Map.size $ driver Map.empty origin up effect
     putStr $ draw $ driver (Map.singleton origin 1) origin up effect

draw :: Map C Integer -> String
draw pixels =
  unlines
    [ [ if Map.lookup (x,y) pixels == Just 1 then '█' else '░' | x <- [minx .. maxx]]
    | y <- [miny .. maxy] ]
  where
    xs   = Map.keys pixels
    minx = minimum (map fst xs)
    miny = minimum (map snd xs)
    maxx = maximum (map fst xs)
    maxy = maximum (map snd xs)

driver :: Map C Integer -> C -> C -> Effect -> Map C Integer
driver paint here dir effect =
  case effect of
    Halt -> paint
    Input f -> driver paint here dir (f (Map.findWithDefault 0 here paint))
    Output color (Output turn e) ->
      driver paint' (add here dir') dir' e
      where
        dir'
          | turn == 0 = turnL dir
          | otherwise = turnR dir
        paint' = Map.insert here color paint
    _ -> error "bad robot"

up, origin :: C
up     = (0,-1)
origin = (0,0)

add :: C -> C -> C
add (x,y) (u,v) = (x+u,y+v)

turnL, turnR :: C -> C
turnL (dx,dy) = (dy,-dx)
turnR (dx,dy) = (-dy,dx)
