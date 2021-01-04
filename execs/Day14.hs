{-# Language ImportQualifiedPost, OverloadedStrings, NumericUnderscores #-}
{-|
Module      : Main
Description : Day 14 solution
Copyright   : (c) Eric Mertens, 2019
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2019/day/14>

-}
module Main (main) where

import Advent (Parser, getParsedLines, number, satisfy, sepBy)
import Control.Applicative (some)
import Data.List (foldl', sortOn)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Char (isAlpha)

p2ore :: Integer
p2ore = 1_000_000_000_000

-- Input file parser ---------------------------------------------------

type Reaction = ([Component], Component)
type Component = (Integer, String)

parseChemical :: Parser String
parseChemical = some (satisfy isAlpha)

parseItem :: Parser Component
parseItem = (,) <$> number <* " " <*> parseChemical

parseItems :: Parser [Component]
parseItems = parseItem `sepBy` ", "

parseReaction :: Parser Reaction
parseReaction = (,) <$> parseItems <* " => " <*> parseItem

------------------------------------------------------------------------

type Step = (String, Integer, Map String Integer)

-- |
-- :main
-- 751038
-- 2074843
main :: IO ()
main =
  do inp <- getParsedLines 14 parseReaction
     let recipes = arrange inp

     print (oreNeeded divUp recipes 1)

     let optimal = fromInteger p2ore / oreNeeded (/) recipes 1 :: Rational
     print $ until (\i -> oreNeeded divUp recipes i <= p2ore) (subtract 1)
           $ truncate optimal

-- | Determine the amount of ORE needed to create the given amount of FUEL
oreNeeded ::
  Num a =>
  (a -> a -> a) {- ^ division                -} ->
  [Step]        {- ^ arranged reaction steps -} ->
  a             {- ^ fuel amount             -} ->
  a             {- ^ ore amount              -}
oreNeeded divide recipes n =
  foldl' (doStep divide) (Map.singleton "FUEL" n) recipes Map.! "ORE"

-- | Use the given reaction step to reduce any of that item needed into
-- simpler components.
doStep ::
  Num a =>
  (a -> a -> a) {- ^ division                     -} ->
  Map String a  {- ^ items needed before reaction -} ->
  Step          {- ^ reaction step                -} ->
  Map String a  {- ^ items needed after reaction  -}
doStep divide need (item, makes, needs) =
  case Map.lookup item need of
    Nothing  -> need
    Just qty -> Map.unionWith (+) need1 need2
      where
        reactions = qty `divide` fromInteger makes
        need1 = Map.delete item need
        need2 = (\i -> reactions * fromInteger i) <$> needs

-- | Sort the reaction steps topologically so that earlier steps only
-- rely on later steps.
arrange :: [Reaction] -> [Step]
arrange xs = sortOn (negate . depth . key) out
  where
    key (x,_,_) = x
    out        = [(rhs, n, Map.fromList [(name,m)|(m,name)<-lhs]) | (lhs,(n,rhs)) <- xs]
    depthMap   = Map.fromList (("ORE", 0) : [(dst, toDepth src)  | (src,(_,dst)) <- xs])
    depth      = (depthMap Map.!)
    toDepth [] = 0 :: Int
    toDepth ys = 1 + maximum (depth . snd <$> ys)

-- | Integer division that rounds up instead of down.
divUp :: Integer -> Integer -> Integer
x `divUp` y = (x + y - 1) `div` y
