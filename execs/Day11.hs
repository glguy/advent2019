{-# Language ImportQualifiedPost #-}
{-|
Module      : Main
Description : Day 11 solution
Copyright   : (c) Eric Mertens, 2019
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2019/day/11>

-}
module Main (main) where

import Advent (getIntcodeInput)
import Advent.Coord (Coord, turnLeft, turnRight, addCoord, origin, north, drawCoords)
import Data.Map (Map)
import Data.Map qualified as Map
import Intcode (intcodeToList)

main :: IO ()
main =
  do inp <- getIntcodeInput 11
     let render = putStrLn . drawCoords . fmap paintChar
         world1 = runner inp Map.empty
         world2 = runner inp (Map.singleton origin 1)

     print (Map.size world1)
     render world2

runner ::
  [Int]         {- ^ intcode program -} ->
  Map Coord Int {- ^ initial world   -} ->
  Map Coord Int {- ^ final world     -}
runner inp world0 = last [w | (_,_,w) <- states]
  where
    inputs  = [Map.findWithDefault 0 here world | (_, here, world) <- states]
    outputs = intcodeToList inp inputs
    states  = (north, origin, world0) : zipWith robotStep states (pairs outputs)

-- | Apply the robot movement logic to the current robot state
robotStep ::
  (Coord, Coord, Map Coord Int) {- ^ vector, location, world -} ->
  (Int,Int)                     {- ^ robot's command         -} ->
  (Coord, Coord, Map Coord Int) {- ^ vector, location, world -}
robotStep (dir, here, world) (color, turn) = (dir', here', world')
  where
    world' = Map.insert here color world
    dir'   = turnFn turn dir
    here'  = addCoord here dir'

-- | Compute the turn function given a robot's output.
turnFn :: Int {- ^ robot turn output -} -> Coord -> Coord
turnFn 0 = turnLeft
turnFn 1 = turnRight
turnFn x = error ("Unexpected turn command: " ++ show x)

-- | Character representation of paint number.
paintChar :: Int -> Char
paintChar 0 = '░'
paintChar 1 = '█'
paintChar x = error ("Unexpected paint color: " ++ show x)

pairs :: [a] -> [(a,a)]
pairs (x:y:z) = (x,y) : pairs z
pairs _       = []
