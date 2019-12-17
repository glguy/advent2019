{-|
Module      : Main
Description : Day 17 solution
Copyright   : (c) Eric Mertens, 2019
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2019/day/17>

-}
module Main (main) where

import           Advent
import           Advent.Intcode
import           Advent.Coord
import           Data.Char
import           Data.Map (Map)
import qualified Data.Map as Map

main :: IO ()
main =
  do [inp] <- getParsedLines 17 memoryParser

     let ascii = map (chr . fromIntegral) (intCodeToList inp [])
         world = Map.fromList [ (C y x, col)
                                | (y,row) <- zip [0..] (lines ascii)
                                , (x,col) <- zip [0..] row ]

     print $ sum [ coordRow k * coordCol k
                 | k <- Map.keys world
                 , all (\x -> '#' == at x world) (k : cardinal k) ]

     print $ last $ effectList (run (set 0 2 (new inp)))
                  $ map (fromIntegral . ord) script

script :: String
script = unlines [
  "A,B,A,C,B,C,A,B,A,C",
  "R,10,L,8,R,10,R,4",
  "L,6,L,6,R,10",
  "L,6,R,12,R,12,R,10",
  "n"]

at :: Coord -> Map Coord Char -> Char
at = Map.findWithDefault '.'


{-
let start = head [ k | (k,'^') <- Map.toList world ]
print $ intercalate "," $ "R" : path world start (C 0 1)

path world here dir = next
  where
    endPoint = last steps
    n = length steps - 1
    steps = takeWhile (\x -> at x world /= '.')
                    $ iterate (addCoord dir) here

    next
      | '#' == at (addCoord (turnLeft dir) endPoint) world =
           show n : "L" : path world endPoint (turnLeft dir)
      | '#' == at (addCoord (turnRight dir) endPoint) world =
           show n : "R" : path world endPoint (turnRight dir)
      | otherwise = show n : []

play :: Effect -> IO ()
play (Output i Halt) = putStrLn ("<<" ++ show i ++ ">>")
play (Output o e) = putChar (chr (fromIntegral o)) >> play e
play (Input f) = getChar >>= play . f . fromIntegral . ord
play Halt = return ()

-}
