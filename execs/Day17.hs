{-|
Module      : Main
Description : Day 17 solution
Copyright   : (c) Eric Mertens, 2019
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2019/day/17>

-}
module Main (main) where

import           Advent
import           Advent.Intcode
import           Advent.Coord
import           Data.Char
import           Data.Map (Map)
import qualified Data.Map as Map

import Data.List
import Data.Maybe

main :: IO ()
main =
  do [inp] <- getParsedLines 17 memoryParser
     let ascii = map (chr . fromIntegral) (intCodeToList inp [])
         world = Map.fromList [ (C y x, col)
                                | (y,row) <- zip [0..] (lines ascii)
                                , (x,col) <- zip [0..] row ]

     print $ sum [ coordRow k * coordCol k
                 | k <- Map.keys world
                 , all (\x -> '#' == at x world) (k : cardinal k) ]

     print (part2 inp world)

at :: Coord -> Map Coord Char -> Char
at = Map.findWithDefault '.'

part2 :: [Integer] -> Map Coord Char -> Integer
part2 inp world =
     last $ effectList (run (set 0 2 (new inp)))
          $ map (fromIntegral . ord) input
  where
    start   = head [ k | (k,'^') <- Map.toList world ]
    p       = path world start (C (-1) 0)
    input:_ = compress [] Nothing Nothing Nothing p

data Dir = U | D | L | R
  deriving (Eq, Ord, Show)

path :: Map Coord Char -> Coord -> Coord -> [(Dir,Int)]
path world here dir
  | '#' == at (addCoord (turnLeft  dir) here) world = walk L turnLeft
  | '#' == at (addCoord (turnRight dir) here) world = walk R turnRight
  | otherwise                                           = []
  where
    walk cmd f = (cmd, n) : path world endPoint dir'
      where
        dir'     = f dir
        steps    = takeWhile (\x -> at x world /= '.')
                 $ iterate (addCoord dir') here
        n        = length steps - 1
        endPoint = last steps

compress ::
  [String] ->
  Maybe [(Dir,Int)] ->
  Maybe [(Dir,Int)] ->
  Maybe [(Dir,Int)] ->
  [(Dir,Int)] ->
  [String]
compress acc a b c [] = [unlines [ intercalate "," (reverse acc)
                                 , maybe "L" instructions a
                                 , maybe "L" instructions b
                                 , maybe "L" instructions c, "n"] ]
compress acc a b c xs =
  do (ys,zs) <- splits xs
     [ () | short ys ]
     id [ z | Just ys == a, z <- compress ("A":acc) a b c zs ] ++
        [ z | Just ys == b, z <- compress ("B":acc) a b c zs ] ++
        [ z | Just ys == c, z <- compress ("C":acc) a b c zs ] ++
        [ z | isNothing a,                           z <- compress ("A":acc) (Just ys) b c zs ] ++
        [ z | isJust    a, isNothing b,              z <- compress ("B":acc) a (Just ys) c zs ] ++
        [ z | isJust    a, isJust    b, isNothing c, z <- compress ("C":acc) a b (Just ys) zs ]

short :: [(Dir,Int)] -> Bool
short xs = length (instructions xs) <= 20

instructions :: [(Dir,Int)] -> String
instructions xs = intercalate "," [ show d ++ "," ++ show n | (d,n) <- xs ]

splits :: [a] -> [([a],[a])]
splits xs = tail $ zip (inits xs) (tails xs)

play :: Effect -> IO ()
play (Output i Halt) = putStrLn ("<<" ++ show i ++ ">>")
play (Output o e) = putChar (chr (fromIntegral o)) >> play e
play (Input f) = getChar >>= play . f . fromIntegral . ord
play Halt = return ()
play Fault = return ()
