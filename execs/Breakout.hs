{-# Language OverloadedStrings #-}
{-|
Module      : Main
Description : Day 13 solution
Copyright   : (c) Eric Mertens, 2019
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2019/day/13>

-}
module Main (main) where

import           Advent
import           Advent.Intcode
import           Advent.Coord
import           Data.Map (Map)
import qualified Data.Map as Map

import           System.IO

main :: IO ()
main =
  do [inp] <- getParsedLines 13 memoryParser
     hSetBuffering stdin NoBuffering
     hSetEcho stdin False
     robot Map.empty (run $ set 0 2 $ new inp)

render :: Integer -> Char
render 1 = '█'
render 2 = '■'
render 3 = '-'
render 4 = '○'
render _ = ' '

robot :: Map Coord Integer -> Effect -> IO ()
robot tiles effect = do
  case effect of

    Halt -> return ()

    Output (-1) (Output 0 (Output t e)) ->
      do putStrLn ("Score: " ++ show t)
         robot tiles e
    Output x (Output y (Output t e))
      | t == 0 -> robot (Map.delete p tiles) e
      | otherwise -> robot (Map.insert p t tiles) e
      where p = C (fromIntegral y) (fromIntegral x)

    Input f ->
      do putStr (draw (fmap render tiles))
         c <- getChar
         case c of
           'a' -> robot tiles (f (-1))
           'd' -> robot tiles (f (1))
           _   -> robot tiles (f 0)

    _ -> error "bad program"

draw :: Map Coord Char -> String
draw pixels =
  case  boundingBox (Map.keys pixels) of
    Nothing -> ""
    Just (C miny minx, C maxy maxx) ->
       unlines [[pixel (C y x) | x <- [minx .. maxx]] | y <- [miny .. maxy]]
      where
        pixel c = Map.findWithDefault ' ' c pixels

