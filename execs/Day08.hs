{-# Language ImportQualifiedPost #-}
{-|
Module      : Main
Description : Day 8 solution
Copyright   : (c) Eric Mertens, 2019
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2019/day/8>

-}
module Main (main) where

import Advent (getInputLines, cardinality, chunks)
import Data.List (minimumBy)
import Data.Ord (comparing)
import Data.Map qualified as Map

main :: IO ()
main =
  do [inp] <- getInputLines 8
     let layers = chunks (25*6) inp
     print (part1 layers)
     mapM_ (putStrLn . map render) (chunks 25 (overlayLayers layers))

render :: Char -> Char
render '0' = '\x2591'
render '1' = '\x2588'
render '2' = '\x2592'
render _   = error "bad pixel"

overlayLayers :: [String] -> String
overlayLayers = foldr1 (zipWith overlay)

overlay :: Char -> Char -> Char
overlay '2' x = x
overlay x   _ = x

part1 :: [String] -> Int
part1 = checksum . minimumBy (comparing (ix '0')) . map cardinality
  where
    ix         = Map.findWithDefault 0
    checksum x = ix '1' x * ix '2' x
