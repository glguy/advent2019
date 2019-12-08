{-|
Module      : Main
Description : Day 8 solution
Copyright   : (c) Eric Mertens, 2019
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2019/day/8>

-}
module Main (main) where

import Advent              (getParsedLines, anySingle, count)
import Control.Applicative (many)
import Data.List           (minimumBy)
import Data.Ord            (comparing)

main :: IO ()
main =
  do [inp] <- getParsedLines 8 (many anySingle)
     let layers = chunks 6 (chunks 25 inp)
     print (part1 layers)
     mapM_ (putStrLn . map render) (overlayLayers layers)

render :: Char -> Char
render '1' = 'X'
render _   = ' '

overlayLayers :: [[[Char]]] -> [[Char]]
overlayLayers = foldr1 (zipWith (zipWith overlay))

overlay :: Char -> Char -> Char
overlay '2' y = y
overlay x   _ = x

part1 :: [[[Char]]] -> Int
part1 layers = count ('1'==) layer * count ('2'==) layer
  where
    layer = minimumBy (comparing (count ('0'==)))
          $ map concat layers

chunks :: Int -> [a] -> [[a]]
chunks _ [] = []
chunks n xs =
  case splitAt n xs of
    (a,b) -> a : chunks n b
