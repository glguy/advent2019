{-# Language OverloadedStrings, NumericUnderscores #-}
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

p2ore :: Int
p2ore = 1_000_000_000_000

-- Input file parser ---------------------------------------------------

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

------------------------------------------------------------------------

main :: IO ()
main =
  do inp <- getParsedLines 14 parseReaction
     let recipes = arrange inp

     print (oreNeeded recipes 1)

     let optimal = optimalOrePerFuel recipes
     print $ until (\i -> oreNeeded recipes i <= p2ore) (subtract 1)
           $ truncate (fromIntegral p2ore / optimal)

optimalOrePerFuel :: [Reaction] -> Rational
optimalOrePerFuel r = orePerThing Map.! "FUEL"
  where
    orePerThing = Map.insert "ORE" 1 (Map.fromList [(c, aux b a) | (a,(b,c)) <- r])
    aux n needs = sum [fromIntegral m * orePerThing Map.! thing | (m,thing) <- needs]
                / fromIntegral n

oreNeeded :: [Reaction] -> Int {- ^ fuel amount -} -> Int {- ^ ore amount -}
oreNeeded recipes n =
  foldl' react (Map.singleton "FUEL" n) recipes Map.! "ORE"

react ::
  Map String Int {- ^ items needed  -} ->
  Reaction       {- ^ item to react -} ->
  Map String Int {- ^ items needed  -}
react need (needs, (makes, item)) = Map.unionWith (+) need1 need2
  where
    needed = Map.findWithDefault 0 item need

    n = needed `divUp` makes

    need1 = Map.delete item need
    need2 = Map.fromListWith (+) [(k,n*v) | (v,k) <- needs]

arrange :: [Reaction] -> [Reaction]
arrange xs = sortOn (negate . depth . snd . snd) xs
  where
    partsMap       = Map.fromList [ (dst, (n, src))  | (src,(n,dst)) <- xs ]
    toDepth (_,ys) = foldl' max (-1) [depth y | (_,y) <- ys] + 1
    depthMap       = Map.insert "ORE" (0 :: Integer) (toDepth <$> partsMap)
    depth          = (depthMap Map.!)

-- | Integer division that rounds up instead of down.
divUp :: Integral a => a -> a -> a
x `divUp` y = (x + y - 1) `div` y
