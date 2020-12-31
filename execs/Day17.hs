{-# Language BlockArguments, ImportQualifiedPost, TransformListComp #-}
{-|
Module      : Main
Description : Day 17 solution
Copyright   : (c) Eric Mertens, 2019
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2019/day/17>

-}
module Main (main) where

import Advent (getIntcodeInput)
import Advent.Coord
import Data.Char (chr, ord)
import Data.List (intersperse, stripPrefix, intercalate, inits, tails)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (maybeToList)
import Intcode (intcodeToList, effectList, run, new, set)

--
-- >>> :main
-- 7280
-- 1045393
main :: IO ()
main =
  do inp <- getIntcodeInput 17
     let ascii = map chr (intcodeToList inp [])
         world = Map.fromList [ (C y x, col)
                                | (y,row) <- zip [0..] (lines ascii)
                                , (x,col) <- zip [0..] row, col /= '.' ]

     print (part1 world)
     print (part2 inp world)

part1 :: Map Coord Char -> Int
part1 world =
  sum [ coordRow k * coordCol k
      | k <- Map.keys world
      , all (`Map.member` world) (cardinal k) ]

part2 ::
  [Int]          {- ^ intcode program      -} ->
  Map Coord Char {- ^ world map            -} ->
  Int            {- ^ final program output -}
part2 inp world = last (effectList (run (set 0 2 (new inp))) (map ord input))
  where
    start = head [k | (k,'^') <- Map.toList world]
    p     = path world start north
    input = head (search 'A' [] [] p)

data Dir = L | R deriving (Eq, Show)
type Cmds = [(Dir,Int)]

-- Determine that path that takes us from the start to the end
path ::
  Map Coord Char {- ^ world map        -} ->
  Coord          {- ^ current location -} ->
  Coord          {- ^ travel vector    -} ->
  Cmds
path world here dir
  | Map.member (addCoord (turnLeft  dir) here) world = walk L turnLeft
  | Map.member (addCoord (turnRight dir) here) world = walk R turnRight
  | otherwise                                        = []
  where
    walk cmd f = (cmd, n) : path world endPoint dir'
      where
        dir'     = f dir
        steps    = takeWhile (`Map.member` world)
                 $ iterate (addCoord dir') here
        n        = length steps - 1
        endPoint = last steps


search ::
  Char     {- ^ next subroutine name  -} ->
  [Cmds]   {- ^ current subroutines   -} ->
  [Char]   {- ^ reversed main program -} ->
  Cmds     {- ^ remaining path        -} ->
  [String] {- ^ program input         -}
search _ subs pgm [] =
  [ unlines
  $ intersperse ',' (reverse pgm)
  : take 3 (map instructions subs ++ repeat "L")
  ++ ["n"]
  ]
search next subs pgm p = oldSub ++ newSub
  where
    oldSub = [ answer
             | (name, sub) <- zip ['A'..] subs
             , path'       <- maybeToList (stripPrefix sub p)
             , answer      <- search next subs (name:pgm) path' ]

    newSub = [ answer
             | next < 'D'
             , (sub,rest)  <- tail (splits p), then takeWhile by short sub
             , answer      <- search (succ next) (subs ++ [sub]) (next:pgm) rest ]

-- subroutines are limited to be at most 20 characters
short :: Cmds -> Bool
short xs = length (instructions xs) <= 20

instructions :: Cmds -> String
instructions xs = intercalate "," do (d,n) <- xs; [show d, show n]

splits :: [a] -> [([a],[a])]
splits xs = zip (inits xs) (tails xs)
