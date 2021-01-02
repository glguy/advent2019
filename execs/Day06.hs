{-# Language ImportQualifiedPost, OverloadedStrings #-}
{-|
Module      : Main
Description : Day 6 solution
Copyright   : (c) Eric Mertens, 2019
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2019/day/6>

-}
module Main (main) where

import Advent (Parser, getParsedLines, satisfy)
import Control.Applicative (many)
import Data.Map qualified as Map

parseName :: Parser String
parseName = many (satisfy (')'/=))

parseOrbit :: Parser (String, String)
parseOrbit = (,) <$> parseName <* ")" <*> parseName

main :: IO ()
main =
  do inp <- getParsedLines 6 parseOrbit
     let paths = Map.fromList [(y,x:Map.findWithDefault [] x paths) | (x,y) <- inp]
     print (sum (length <$> paths))

     -- routes from COM leading to YOU and SAN
     let t1 = reverse (paths Map.! "YOU")
         t2 = reverse (paths Map.! "SAN")
     print (part2 t1 t2)

-- remove the common prefix and then return the length of the remainder
part2 :: Eq a => [a] -> [a] -> Int
part2 (x:xs) (y:ys) | x == y = part2 xs ys
part2 xs     ys              = length xs + length ys
