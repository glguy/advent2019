{-|
Module      : Main
Description : Day 25 solution
Copyright   : (c) Eric Mertens, 2019
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2019/day/25>

@
    #-# #-#
      | |
      # #
      | |
    #-#-S-#-#-#
          |
        #-#-#
            |
          #-#-#-#
                |
                #
@
-}
module Main (main) where

import Advent        (getIntcodeInput)
import Data.Char     (ord, chr)
import Data.Foldable (traverse_)
import Intcode

main :: IO ()
main =
  do inp <- getIntcodeInput 25
     --traverse_ putStrLn (asciiComputer inp direct)
     traverse_ putStrLn (asciiComputer inp search)

asciiComputer :: [Int] -> [String] -> [String]
asciiComputer inp cmds =
  lines $ map chr $ intcodeToList inp $ map ord $ unlines cmds

search :: [String]
search =
  [ north,                             take_ "sand"
  , north,                             take_ "space heater"
  , east ,                             take_ "semiconductor"
  , west , south, south, east ,        take_ "ornament"
  , east , east , west , west , south, take_ "festive hat"
  , east ,                             take_ "asterisk"
  , south, east ,                      take_ "cake"
  , east {- electromagnet -}
  , south, north, west , west , west , take_ "food ration"
  , east , north, west , west {- photons -}
  , east , north, west , west {- molten lava -}
  , west , east , north {- infinite loop -}
  , north] ++ graycode west drop_ take_ items

graycode ::
  a        {- attempt             -} ->
  (a -> a) {- action              -} ->
  (a -> a) {- inverse action      -} ->
  [a]      {- items               -} ->
  [a]      {- search instructions -}
graycode tick f1 f2 xs = foldr step base xs f1 []
  where
    base      = \_ -> (tick:)
    step x go = \f -> go f1 . (f x :) . go f2

items :: [String]
items =
  [ "cake"
  , "sand"
  , "asterisk"
  , "ornament"
  , "festive hat"
  , "food ration"
  , "space heater"
  , "semiconductor"
  ]

{-
direct :: [String]
direct =
  [north, north, take_ "space heater",
   east, take_ "semiconductor",
   west, south, south, east, take_ "ornament",
   south, take_ "festive hat",
   north, west, west, north, north, west]
-}

north, south, east, west :: String
north = "north"
south = "south"
east  = "east"
west  = "west"

take_ :: String -> String
take_ x = "take " ++ x

drop_ :: String -> String
drop_ x = "drop " ++ x
