module Advent.Intcode.Conduino
  ( -- * Intcode adapters
    machinePipe, effectPipe,

    -- * Termination condition
    Outcome(..), isOK,
  ) where

import Advent.Intcode (Machine, Effect(..), run)
import Data.Conduino  (Pipe, awaitSurely, yield)
import Data.Void      (Void)

data Outcome = OutcomeOK | OutcomeFault
  deriving (Eq, Ord, Read, Show)

isOK :: Outcome -> Bool
isOK OutcomeOK    = True
isOK OutcomeFault = False

machinePipe :: Machine -> Pipe Integer Integer Void m Outcome
machinePipe = effectPipe . run

effectPipe :: Effect -> Pipe Integer Integer Void m Outcome
effectPipe Halt         = return OutcomeOK
effectPipe Fault        = return OutcomeFault
effectPipe (Output o e) = yield o     >>  effectPipe e
effectPipe (Input  f  ) = awaitSurely >>= effectPipe . f
