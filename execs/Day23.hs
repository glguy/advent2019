{-|
Module      : Main
Description : Day 23 solution
Copyright   : (c) Eric Mertens, 2019
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2019/day/23>

-}
module Main (main) where

import           Advent
import           Advent.Intcode (Effect(..), memoryParser, run, new, feedInput)
import           Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

main :: IO ()
main =
  do [inp] <- getParsedLines 23 memoryParser
     let net = IntMap.fromList [ (i, feedInput [i] (run (new inp))) | i <- [0..49]]
     let ys = runNetwork 0 0 net
     print (head ys)
     print (firstMatch ys)

firstMatch :: Eq a => [a] -> a
firstMatch ys = head [ a | (a,b) <- zip ys (tail ys), a==b ]

data Packet = Packet Int Int Int
type Network = IntMap Effect

isInput :: Effect -> Bool
isInput Input{} = True
isInput _       = False

gatherPacket :: Effect -> ([Packet], Effect)
gatherPacket (Output dst (Output x (Output y e))) = ([Packet dst x y], e)
gatherPacket e                                    = ([], e)

runNetwork :: Int -> Int -> Network -> [Int]
runNetwork x y net =
  case traverse gatherPacket net of
    ([], comps')
      | all isInput next -> y : runNetwork x y (send1 (Packet 0 x y) comps')
      | otherwise        ->     runNetwork x y next
      where next = fmap (feedInput [-1]) net
    (packets, comps')    -> send x y packets comps'

send :: Int -> Int -> [Packet] -> Network -> [Int]
send nx ny []                         = runNetwork nx ny
send _ _   (Packet 255 x y : packets) = send x y packets
send nx ny (p              : packets) = send nx ny packets
                                      . send1 p

send1 :: Packet -> Network -> Network
send1 (Packet dst x y) = IntMap.update (Just . feedInput [x,y]) dst
