{-# Language OverloadedStrings #-}
{-|
Module      : Main
Description : Day 8 solution
Copyright   : (c) Eric Mertens, 2019
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2019/day/8>

-}
module Main (main) where

import Advent              (Parser, getParsedLines, count)
import Control.Applicative ((<|>),many)
import Data.List           (minimumBy)
import Data.Ord            (comparing)

type Layer = [[Pixel]]

data Pixel = Black | White | Transparent
  deriving (Eq, Ord, Show)

parsePixel :: Parser Pixel
parsePixel = Black <$ "0" <|> White <$ "1" <|> Transparent <$ "2"

main :: IO ()
main =
  do [inp] <- getParsedLines 8 (many parsePixel)
     let layers = chunks 6 (chunks 25 inp)
     print (part1 layers)
     mapM_ (putStrLn . map render) (overlayLayers layers)

render :: Pixel -> Char
render Black       = '\x2591'
render Transparent = '\x2592'
render White       = '\x2588'

overlayLayers :: [Layer] -> Layer
overlayLayers = foldr1 (zipWith (zipWith overlay))

overlay :: Pixel -> Pixel -> Pixel
overlay Transparent x = x
overlay x           _ = x

part1 :: [Layer] -> Int
part1 layers = count (White==) layer * count (Transparent==) layer
  where
    layer = minimumBy (comparing (count (Black==)))
          $ map concat layers

chunks :: Int -> [a] -> [[a]]
chunks _ [] = []
chunks n xs =
  case splitAt n xs of
    (a,b) -> a : chunks n b
