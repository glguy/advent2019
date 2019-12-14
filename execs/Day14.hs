{-# Language OverloadedStrings #-}
{-|
Module      : Main
Description : Day 14 solution
Copyright   : (c) Eric Mertens, 2019
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2019/day/14>

-}
module Main (main) where

import           Advent
import           Control.Applicative
import           Data.List
import qualified Data.Map as Map
import           Data.Char (isAlpha)
import           Data.Maybe (fromJust)
import           Data.Graph.Inductive


parseItem :: Parser (Int, String)
parseItem = (,) <$> number <* " " <*> many (satisfy isAlpha)

parseItems :: Parser [(Int, String)]
parseItems = parseItem `sepBy` ", "

parseFormula :: Parser ([(Int, String)], (Int, String))
parseFormula = (,) <$> parseItems <* " => " <*> parseItem

main :: IO ()
main =
  do inp <- getParsedLines 14 parseFormula

     print (oreNeeded inp 1)

     let p i = oreNeeded inp i <= 1000000000000
     print (binSearch p 1)


binSearch :: (Int -> Bool) -> Int -> Int
binSearch p lo = go (lo+1)
  where
    go hi
      | p hi = go (2*hi)
      | otherwise = binSearch2 p lo hi

binSearch2 :: (Int -> Bool) -> Int -> Int -> Int
binSearch2 p lo hi
  | lo + 1 == hi = lo
  | p mid        = binSearch2 p mid hi
  | otherwise    = binSearch2 p lo mid
  where
    mid = lo + (hi - lo) `div` 2

oreNeeded inp n =
  let m = Map.fromList [ (dst, (n, src))  | (src,(n,dst)) <- inp ] in
  foldl' (compute m) (Map.singleton "FUEL" n) (recipeOrder inp) Map.! "ORE"

compute recipes need item = Map.unionWith (+) need1 need2
  where
    needed = need Map.! item
    need1 = Map.delete item need

    (makes, needs) = recipes Map.! item
    n = (needed + makes - 1) `div` makes
    need2 = Map.fromList [ (k,n*v) | (v,k) <- needs ]


recipeOrder xs = init (map (names!!) (topsort g))
  where
    names = [ name | (_,(_,name)) <- xs ] ++ ["ORE"]
    toId name = fromJust (elemIndex name names)

    g :: UGr
    g = mkUGraph [0..length names-1]
                 [(toId src, toId dst) | (es,(_,src)) <- xs, (_, dst) <- es ]
