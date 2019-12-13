{-# Language BlockArguments, OverloadedStrings #-}
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
import           Advent.Intcode as I
import           Advent.Coord
import           Control.Exception
import           Data.Map (Map)
import qualified Data.Map as Map
import           Graphics.Vty


main :: IO ()
main =
  do [inp] <- getParsedLines 13 memoryParser
     cfg   <- standardIOConfig
     score <- bracket (mkVty cfg) shutdown \vty ->
                game vty 0 Map.empty (run (set 0 2 (new inp)))
     putStrLn ("Final score: " ++ show score)


render :: Integer -> Char
render 1 = '█'
render 2 = '❑'
render 3 = '―'
render 4 = '●'
render _ = ' '

game :: Vty -> Integer -> Map Coord Char -> Effect -> IO Integer
game vty score tiles effect =
  case effect of

    Halt -> return score

    I.Output x (I.Output y (I.Output t e))
      | -1 == x, 0 == y -> game vty t tiles e
      | t == 0          -> game vty score (Map.delete p tiles) e
      | otherwise       -> game vty score ((Map.insert p $! render t) tiles) e
      where p = C (fromIntegral y) (fromIntegral x)

    I.Input f ->
      do update vty (picForImage (draw score tiles))
         e <- nextEvent vty
         case e of
           EvKey KLeft  [] -> game vty score tiles (f (-1))
           EvKey KRight [] -> game vty score tiles (f 1)
           _               -> game vty score tiles (f 0)

    _ -> error "bad program"

draw :: Integer -> Map Coord Char -> Image
draw score pixels =
  case boundingBox (Map.keys pixels) of
    Nothing -> emptyImage
    Just (C miny minx, C maxy maxx) ->
      string defAttr ("Score: " ++ show score) <->
      vertCat [ horizCat [pixel (C y x) | x <- [minx .. maxx]] | y <- [miny .. maxy]]
      where
        pixel c = char defAttr (Map.findWithDefault ' ' c pixels)
