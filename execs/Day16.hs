{-|
Module      : Main
Description : Day 16 solution
Copyright   : (c) Eric Mertens, 2019
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2019/day/16>

This code is not fast but if you let it run it will finish.
I still want to figure out a more efficient solution.

-}
module Main (main) where

import           Advent
import           Control.Applicative
import qualified Data.Vector.Unboxed as V

main :: IO ()
main =
  do [inp] <- getParsedLines 16 (many anySingle)
     let ns = digits inp

     putStrLn $ concatMap show $ V.toList $ V.take 8 $ iterate fft ns !! 100

     let offset = read (take 7 inp)

     let ns' = V.concat (replicate 10000 ns)
     putStrLn $ concatMap show $ V.toList $ V.take 8 $ V.drop offset $ iterate fft ns' !! 100

digits :: String -> V.Vector Int
digits = V.fromList . map (read . pure)

fft :: V.Vector Int -> V.Vector Int
fft xs = V.generate (V.length xs) one
  where
    n = V.length xs
    ps = V.scanl (+) 0 xs
    factors i = takeWhile (\(Region _ a _) -> a < n) (regions i)

    one i = abs $ sum [ m * (ps V.! min n end - ps V.! start)     | Region m start end <- factors i]
               `rem` 10

data Region = Region !Int !Int !Int
  deriving Show

regions :: Int -> [Region]
regions i = go 0
  where
    n = i + 1
    go offset = Region 1 (offset + i) (offset + i+n)
              : Region (-1) (offset + i + n*2) (offset + i+n*2+ n)
              : go (offset + 4 * n)
