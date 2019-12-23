{-|
Module      : Main
Description : Day 23 solution
Copyright   : (c) Eric Mertens, 2019
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2019/day/23>

-}
module Main (main) where

import           Advent         (getParsedLines)
import           Advent.Intcode (Effect(..), memoryParser, feedInput, run, new)
import           Data.IntMap    (IntMap)
import qualified Data.IntMap as IntMap

main :: IO ()
main =
  do [inp] <- getParsedLines 23 memoryParser
     let net    = IntMap.fromList [ (i, feedInput [i] (run (new inp))) | i <- [0..49]]
     let events = runSystem System{ network = net, netX = 0, netY = 0}
     print (head       [y | SetY  y <- events])
     print (firstMatch [y | SendY y <- events])

firstMatch :: Eq a => [a] -> a
firstMatch ys = head [ a | (a,b) <- zip ys (tail ys), a==b ]

data Packet = Packet Int Int Int -- ^ destination, x, y

-- | Map of VM identities to current execution state.
type Network = IntMap Effect

-- | State of network simulation including most recent NAT packet.
data System = System
  { network :: !Network
  , netX    :: !Int
  , netY    :: !Int
  }

-- | Network events needed to answer part 1 and 2.
data Event
  = SetY  !Int -- ^ Y value sent to address 255
  | SendY !Int -- ^ NAT packet set after a system stall

isInput :: Effect -> Bool
isInput Input{} = True
isInput _       = False

-- | Run a VM gathering packets until it blocks waiting for input.
gatherPacket :: Effect -> ([Packet], Effect)
gatherPacket (Output dst (Output x (Output y e))) = do ([Packet dst x y], ()); gatherPacket e
gatherPacket e                                    = pure e

runSystem :: System -> [Event]
runSystem sys =
  case traverse gatherPacket (network sys) of
    (packets, net1)
      | not (null packets) -> send packets sys{ network = net1 }
      | all isInput net2   -> SendY (netY sys)
                            : runSystem sys{ network = deliver natPacket net1 }
      | otherwise          -> runSystem sys{ network = net2 }
      where
        net2      = feedInput [-1] <$> net1
        natPacket = Packet 0 (netX sys) (netY sys)

send :: [Packet] -> System -> [Event]
send []                    sys = runSystem sys
send (Packet 255 x y : ps) sys = SetY y : send ps sys{ netX = x, netY = y }
send (p              : ps) sys = send ps sys{ network = deliver p (network sys) }

deliver :: Packet -> Network -> Network
deliver (Packet dst x y) = IntMap.adjust (feedInput [x,y]) dst
