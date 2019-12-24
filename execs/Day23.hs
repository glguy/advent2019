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
import           Advent.Queue   (Queue((:<|)))
import qualified Advent.Queue as Queue
import           Advent.Intcode (Effect(..), memoryParser, feedInput, run, new)
import           Data.IntMap    (IntMap)
import qualified Data.IntMap as IntMap

main :: IO ()
main =
  do [inp] <- getParsedLines 23 memoryParser
     let events = startup (run (new inp))
     print (head       [y | SetY  y <- events])
     print (firstMatch [y | SendY y <- events])

firstMatch :: Eq a => [a] -> a
firstMatch ys = head [ a | (a,b) <- zip ys (tail ys), a==b ]

data Packet = Packet !Int !Int !Int -- ^ destination, x, y

-- | Map of VM identities to current execution state.
type Network = IntMap Effect

-- | State of network simulation including most recent NAT packet.
data System = System
  { network :: Network
  , nat     :: Maybe (Int,Int)
  , sendq   :: Queue Packet
  }

-- | Network events needed to answer part 1 and 2.
data Event
  = SetY  !Int -- ^ Y value sent to address 255
  | SendY !Int -- ^ NAT packet set after a system stall

-- | Run a VM gathering packets until it blocks waiting for input.
gather :: Effect -> ([Packet], Effect)
gather (Output dst (Output x (Output y e))) = do ([Packet dst x y], ()); gather e
gather e                                    = pure e

-- gather up any packets ready to send at the outset
startup :: Effect -> [Event]
startup eff = stepNetwork (traverse gather)
  System { network = IntMap.fromList [ (i, feedInput [i] eff) | i <- [0..49]]
         , sendq   = Queue.Empty
         , nat     = Nothing }

tryToSend :: System -> [Event]
tryToSend sys
  | p :<| ps   <- sendq sys = deliver p sys{ sendq = ps }
  | Just (x,y) <- nat   sys = SendY y : deliver (Packet 0 x y) sys
  | otherwise               = stepNetwork (traverse (gather . feedInput [-1])) sys

deliver :: Packet -> System -> [Event]
deliver (Packet 255 x y) sys = SetY y : tryToSend sys{ nat = Just (x,y) }
deliver (Packet dst x y) sys = stepNetwork (updateF (gather . feedInput [x,y]) dst) sys

stepNetwork :: (Network -> ([Packet], Network)) -> System -> [Event]
stepNetwork f sys =
  case f (network sys) of
    (ps, net) -> tryToSend sys{ network = net, sendq = Queue.appendList ps (sendq sys) }

updateF :: Applicative f => (a -> f a) -> Int -> IntMap a -> f (IntMap a)
updateF = IntMap.alterF . traverse
