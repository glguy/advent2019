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
import           Data.Set (Set)
import qualified Data.Set as Set

type C = (Int,Int)


main :: IO ()
main =
  do [inp] <- getParsedLines 11 memoryParser
     let effect = run (new inp)
     print $ Set.size $ fst $ driver Set.empty Set.empty origin up effect
     putStr $ draw $ snd $ driver Set.empty (Set.singleton origin) origin up effect

draw :: Set C -> String
draw pixels =
  unlines
    [ [ if Set.member (x,y) pixels then '█' else '░' | x <- [minx .. maxx]]
    | y <- [miny .. maxy] ]
  where
    xs   = Set.toList pixels
    minx = minimum (map fst xs)
    miny = minimum (map snd xs)
    maxx = maximum (map fst xs)
    maxy = maximum (map snd xs)

driver :: Set C -> Set C -> C -> C -> Effect -> (Set C, Set C)
driver pans whites here dir effect =
  case effect of
    Halt -> (pans, whites)
    Input f -> driver pans whites here dir (f (if Set.member here whites then 1 else 0))
    Output color (Output turn e) ->
      let dir'
            | turn == 0 = turnL dir
            | otherwise = turnR dir
          whites'
            | color == 1 = Set.insert here whites
            | otherwise  = Set.delete here whites
      in
      driver (Set.insert here pans) whites' (add here dir') dir' e
    _ -> error "bad robot"

up, origin :: C
up     = (0,-1)
origin = (0,0)

add :: C -> C -> C
add (x,y) (u,v) = (x+u,y+v)

turnL, turnR :: C -> C
turnL (dx,dy) = (dy,-dx)
turnR (dx,dy) = (-dy,dx)
