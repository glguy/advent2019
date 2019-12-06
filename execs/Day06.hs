{-# Language OverloadedStrings #-}
{-|
Module      : Main
Description : Day 6 solution
Copyright   : (c) Eric Mertens, 2019
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2019/day/6>

-}
module Main (main) where

import           Advent
import           Control.Applicative
import           Data.List
import           Data.Map (Map)
import qualified Data.Map as Map

parseName :: Parser String
parseName = many (satisfy (')'/=))

parseOrbit :: Parser (String, String)
parseOrbit = (,) <$> parseName <* ")" <*> parseName

main :: IO ()
main =
  do inp <- getParsedLines 6 parseOrbit

     let orbits = Map.fromList [ (y,x) | (x,y) <- inp]

     print $ sum [ length (path orbits i) | (_,i) <- inp ]

     let you    = orbits Map.! "YOU"
         san    = orbits Map.! "SAN"
         t1     = path orbits you
         t2     = path orbits san
         common = intersect t1 t2

     print (length t1 + length t2 - 2 * length common)

path :: Ord a => Map a a -> a -> [a]
path m i =
  case Map.lookup i m of
    Nothing -> []
    Just j  -> i : path m j
