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

import Advent (Parser, satisfy, number, sepBy, getParsedLines)
import Control.Applicative (some)
import Data.List (foldl', sortOn)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Char (isAlpha)
import Data.Ratio ((%))

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

main :: IO ()
main =
  do inp <- getParsedLines 14 parseReaction
     let recipes = arrange inp

     print (oreNeeded recipes 1)

     let optimal = optimalOrePerFuel recipes
     print $ until (\i -> oreNeeded recipes i <= p2ore) (subtract 1)
           $ truncate (fromIntegral p2ore / optimal)

-- | Compute the amount of ORE needed to construct a single FUEL when
-- fractional units are possible.
optimalOrePerFuel :: [Step] -> Rational
optimalOrePerFuel r = orePerThing Map.! "FUEL"
  where
    orePerThing = Map.fromList (("ORE",1):[(c, aux b a) | (c,b,a) <- r])
    aux n needs = sum (Map.intersectionWith (\x y -> x * (y%n)) orePerThing needs)

-- | Determine the amount of ORE needed to create the given amount of FUEL
oreNeeded ::
  [Step]  {- ^ arranged reaction steps -} ->
  Integer {- ^ fuel amount -} ->
  Integer {- ^ ore amount  -}
oreNeeded recipes n = foldl' step (Map.singleton "FUEL" n) recipes Map.! "ORE"

-- | Use the given reaction step to reduce any of that item needed into
-- simpler components.
step ::
  Map String Integer {- ^ items needed before reaction -} ->
  Step               {- ^ reaction step                -} ->
  Map String Integer {- ^ items needed after reaction  -}
step need (item, makes, needs) =
  case Map.lookup item need of
    Nothing  -> need
    Just qty -> Map.unionWith (+) need1 need2
      where
        reactions = qty `divUp` makes
        need1 = Map.delete item need
        need2 = (reactions*) <$> needs

-- | Sort the reaction steps topologically so that earlier steps only
-- rely on later steps.
arrange :: [Reaction] -> [Step]
arrange xs = sortOn (negate . depth . (\(x,_,_)->x)) out
  where
    out        = [(rhs,n,Map.fromList [(name,m)|(m,name)<-lhs]) | (lhs,(n,rhs)) <- xs]
    depthMap   = Map.fromList (("ORE", 0::Int) : [(dst, toDepth src)  | (src,(_,dst)) <- xs])
    depth      = (depthMap Map.!)
    toDepth [] = 0
    toDepth ys = 1 + maximum (depth . snd <$> ys)

-- | Integer division that rounds up instead of down.
divUp :: Integer -> Integer -> Integer
x `divUp` y = (x + y - 1) `div` y
