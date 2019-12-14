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
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Char (isAlpha)

data Recipes = Recipes
  { order :: [String]
  , parts :: Map String (Int, [(Int, String)])
  }

type Reaction = ([Component], Component)
type Component = (Int, String)

parseChemical :: Parser String
parseChemical = some (satisfy isAlpha)

parseItem :: Parser Component
parseItem = (,) <$> number <* " " <*> parseChemical

parseItems :: Parser [Component]
parseItems = parseItem `sepBy` ", "

parseReaction :: Parser Reaction
parseReaction = (,) <$> parseItems <* " => " <*> parseItem

main :: IO ()
main =
  do inp <- getParsedLines 14 parseReaction
     let recipes = mkRecipes inp

     print (oreNeeded recipes 1)

     let p i = oreNeeded recipes i <= 1000000000000
     print (binSearch p 1)

oreNeeded :: Recipes -> Int {- ^ fuel amount -} -> Int {- ^ ore amount -}
oreNeeded recipes n =
  foldl' (react recipes) (Map.singleton "FUEL" n) (order recipes) Map.! "ORE"

react ::
  Recipes ->
  Map String Int {- ^ items needed   -} ->
  String         {- ^ items to react -} ->
  Map String Int {- ^ items needed   -}
react recipes need item = Map.unionWith (+) need1 need2
  where
    needed = Map.findWithDefault 0 item need

    (makes, needs) = parts recipes Map.! item
    n = needed `divUp` makes

    need1 = Map.delete item need
    need2 = Map.fromListWith (+) [ (k,n*v) | (v,k) <- needs ]

-- | Integer division that rounds up instead of down.
divUp :: Integral a => a -> a -> a
x `divUp` y = (x + y - 1) `div` y

mkRecipes :: [Reaction] -> Recipes
mkRecipes xs = Recipes
  { order = sortOn depth [n | (_, (_,n)) <- xs ]
  , parts = partsMap
  }
  where
    partsMap       = Map.fromList [ (dst, (n, src))  | (src,(n,dst)) <- xs ]
    toDepth (_,ys) = foldl' min 0 [ depth y | (_,y) <- ys ] - 1
    depthMap       = fmap toDepth partsMap
    depth x        = Map.findWithDefault (0::Int) x depthMap

------------------------------------------------------------------------

binSearch ::
  (Int -> Bool) {- ^ predicate        -} ->
  Int           {- ^ small enough     -} ->
  Int           {- ^ largest possible -}
binSearch p lo = go (lo+1)
  where
    go hi
      | p hi      = go (2*hi)
      | otherwise = binSearch2 p lo hi

binSearch2 ::
  (Int -> Bool) {- ^ predicate    -} ->
  Int           {- ^ small enough -} ->
  Int           {- ^ too big      -} ->
  Int
binSearch2 p lo hi
  | lo + 1 == hi = lo
  | p mid        = binSearch2 p mid hi
  | otherwise    = binSearch2 p lo mid
  where
    mid = lo + (hi - lo) `div` 2
