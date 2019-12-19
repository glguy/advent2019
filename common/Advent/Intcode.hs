{-# Language DeriveTraversable, OverloadedStrings #-}
{-|
Module      : Advent.Intcode
Description : Intcode interpreter
Copyright   : (c) Eric Mertens, 2019
License     : ISC
Maintainer  : emertens@gmail.com

This Intcode interpreter is defined across multiple Advent of Code days:

* <https://adventofcode.com/2019/day/2>
* <https://adventofcode.com/2019/day/5>
* <https://adventofcode.com/2019/day/7>
* <https://adventofcode.com/2019/day/9>

This implementation works with the following passes:

  1. Parse input text file into a list of numbers
  2. Execute op codes to single-step input/output effects.
  3. Execute single-stop effects into big-step effects.
  4. Optional: Evaluate the effect as a function from a list of inputs to list of outputs

>>> intCodeToList [3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9] <$> [[0],[10]]
[[0],[1]]

>>> intCodeToList [3,3,1105,-1,9,1101,0,0,12,4,12,99,1] <$> [[0],[10]]
[[0],[1]]

>>> :{
>>> intCodeToList
>>>   [3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,
>>>    1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,
>>>    999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99]
>>> <$> [[7],[8],[9]]
>>> :}
[[999],[1000],[1001]]

-}
module Advent.Intcode
  (
  -- * Simple interface
  memoryParser,
  intCodeToList,

  -- * Machine state
  Machine(..), (!), new, set, memoryList,

  -- * Effects
  Effect(..), run, (>>>), effectList, followedBy, feedInput,

  -- * Small-step
  Step(..), step,

  -- * Opcodes
  Mode(..), Opcode(..), decode,
  ) where

import           Advent           (Parser, number, sepBy)
import           Data.IntMap         (IntMap)
import           Data.Traversable (mapAccumL)
import qualified Data.IntMap as IntMap
import qualified Data.Vector.Unboxed as V
import           Text.Show.Functions ()
import           Data.Maybe (fromMaybe)

------------------------------------------------------------------------
-- High-level interface
------------------------------------------------------------------------

-- | Run a given memory image as a list transducer.
intCodeToList ::
  [Int] {- ^ initial memory -} ->
  [Int] {- ^ inputs         -} ->
  [Int] {- ^ outputs        -}
intCodeToList = effectList . run . new

-- | Evaluate a program's effect as a function from a list of
-- inputs to a list of outputs.
effectList ::
  Effect {- ^ program effect -} ->
  [Int]  {- ^ inputs         -} ->
  [Int]  {- ^ outputs        -}
effectList effect inputs =
  case effect of
    Input f | x:xs <- inputs -> effectList (f x) xs
            | otherwise      -> error "Not enough inputs"
    Output o e               -> o : effectList e inputs
    Halt                     -> []
    Fault                    -> error "Bad instruction"

------------------------------------------------------------------------
-- Machine state
------------------------------------------------------------------------

-- | Machine state
data Machine = Machine
  { pc      :: !Int          -- ^ program counter
  , relBase :: !Int          -- ^ relative base pointer
  , memory  :: !(IntMap Int) -- ^ program memory
  , image   :: {-# Unpack #-} !(V.Vector Int)
  }
  deriving (Eq, Ord, Show)

-- | Memory lookup from 0-based index
(!) ::
  Machine {- ^ machine  -} ->
  Int     {- ^ position -} ->
  Int     {- ^ value    -}
m ! i = IntMap.findWithDefault def i (memory m)
  where
    def = fromMaybe 0 (image m V.!? i)

-- | Construct machine from a list of initial values starting
-- at address 0. Program counter and relative base start at 0.
new ::
  [Int]   {- ^ initial memory -} ->
  Machine {- ^ new machine    -}
new initialValues = Machine
  { pc      = 0
  , relBase = 0
  , memory  = IntMap.empty
  , image   = V.fromList initialValues
  }

-- | Update the value stored at a given location in memory.
set ::
  Int {- ^ position  -} ->
  Int {- ^ new value -} ->
  Machine -> Machine
set i v m
  | v == def  = m { memory = IntMap.delete i   (memory m) }
  | otherwise = m { memory = IntMap.insert i v (memory m) }
  where
    def = fromMaybe 0 (image m V.!? i)

-- | Update the relative base pointer by adding an offset to it.
adjustRelBase :: Int {- ^ offset -} -> Machine -> Machine
adjustRelBase i mach = mach { relBase = relBase mach + i }

-- | Set program counter to a new address.
jmp ::
  Int {- ^ program counter -} ->
  Machine -> Machine
jmp i mach = mach { pc = i }

-- | Generate a list representation of memory starting from
-- zero. This can get big for sparsely filled memory using
-- large addresses. Returned values start at position 0.
memoryList :: Machine -> [Int]
memoryList mach
  | IntMap.null (memory mach) = []
  | otherwise = map (mach !) [0 .. max (V.length (image mach) - 1)
                                       (fst (IntMap.findMax (memory mach)))]

------------------------------------------------------------------------
-- Parsing
------------------------------------------------------------------------

-- | Parse an Intcode program as a list of comma separated integers.
memoryParser :: Parser [Int]
memoryParser = number `sepBy` ","

------------------------------------------------------------------------
-- Big-step semantics
------------------------------------------------------------------------

-- | Possible effects from running a machine
data Effect
  = Output !Int Effect    -- ^ Output an integer
  | Input (Int -> Effect) -- ^ Input an integer
  | Halt                  -- ^ Halt execution
  | Fault                 -- ^ Execution failure
  deriving Show

-- | Big-step semantics of virtual machine.
run :: Machine -> Effect
run mach =
  case step mach of
    Step mach'        -> run mach'
    StepOut out mach' -> Output out (run mach')
    StepIn f          -> Input (run . f)
    StepHalt          -> Halt
    StepFault         -> Fault

-- | Compose two effects together. Outputs from first argument are
-- used as inputs to the second effect. Composed effect halts when
-- the second machine halts.
(>>>) :: Effect -> Effect -> Effect
x          >>> Output o y = Output o (x >>> y)
_          >>> Halt       = Halt
_          >>> Fault      = Fault
Output o x >>> Input f    = x >>> f o
Halt       >>> Input _    = Fault
Fault      >>> Input _    = Fault
Input f    >>> y          = Input (\i -> f i >>> y)

infixl 9 >>>

-- | Run first effect until it halts, then run the second effect.
followedBy :: Effect -> Effect -> Effect
followedBy Halt         y = y
followedBy Fault        _ = Fault
followedBy (Output o x) y = Output o (followedBy x y)
followedBy (Input  f  ) y = Input (\i -> followedBy (f i) y)

-- | Provide an input to the first occurence of an input request
-- in a program effect. It is considered a fault if a program
-- terminates before using the input.
feedInput :: [Int] -> Effect -> Effect
feedInput []     e            = e
feedInput xs     (Output o e) = Output o (feedInput xs e)
feedInput (x:xs) (Input f)    = feedInput xs (f x)
feedInput _ _                 = Fault

------------------------------------------------------------------------
-- Small-step semantics
------------------------------------------------------------------------

-- | Result of small-step semantics.
data Step
  = Step    !Machine         -- ^ no effect
  | StepOut !Int !Machine    -- ^ output
  | StepIn  (Int -> Machine) -- ^ input
  | StepHalt                 -- ^ halt
  | StepFault                -- ^ bad instruction
  deriving Show

-- | Small-step semantics of virtual machine.
step :: Machine -> Step
step mach =
  case decode (at (pc mach)) of
    Nothing -> StepFault
    Just opcodeMode ->
      case mapWithIndex toPtr (pc mach + 1) $! opcodeMode of
        (pc', opcode) -> impl opcode $! jmp pc' mach

  where
    at :: Int -> Int
    at i = mach ! i

    toPtr :: Int -> Mode -> Int
    toPtr i Imm =    i
    toPtr i Abs = at i
    toPtr i Rel = at i + relBase mach

    impl opcode =
      case opcode of
        Add a b c -> Step . set c (at a + at b)
        Mul a b c -> Step . set c (at a * at b)
        Inp a     -> StepIn . flip (set a)
        Out a     -> StepOut (at a)
        Jnz a b   -> Step . if at a /= 0 then jmp (at b) else id
        Jz  a b   -> Step . if at a == 0 then jmp (at b) else id
        Lt  a b c -> Step . set c (if at a <  at b then 1 else 0)
        Eq  a b c -> Step . set c (if at a == at b then 1 else 0)
        Arb a     -> Step . adjustRelBase (at a)
        Hlt       -> const StepHalt

mapWithIndex :: (Int -> a -> b) -> Int -> Opcode a -> (Int, Opcode b)
mapWithIndex f = mapAccumL (\i a -> (i+1, f i a))
{-# INLINE mapWithIndex #-}

------------------------------------------------------------------------
-- Opcode decoder
------------------------------------------------------------------------

-- | Parameter modes
data Mode
  = Abs -- ^ absolute position
  | Imm -- ^ immediate
  | Rel -- ^ relative position
  deriving (Eq, Ord, Read, Show)

-- | Opcodes parameterized over argument representations.
data Opcode a
  = Add !a !a !a -- ^ addition:        @c = a + b@
  | Mul !a !a !a -- ^ multiplication:  @c = a * b@
  | Inp !a       -- ^ input:           @a = input()@
  | Out !a       -- ^ output:          @output(a)@
  | Jnz !a !a    -- ^ jump-if-true:    @if a then goto b@
  | Jz  !a !a    -- ^ jump-if-false:   @if !a then goto b@
  | Lt  !a !a !a -- ^ less-than:       @c = a < b@
  | Eq  !a !a !a -- ^ equals:          @c = a == b@
  | Arb !a       -- ^ adjust-rel-base: @rel += a@
  | Hlt          -- ^ halt
  deriving (Eq, Ord, Read, Show, Functor, Foldable)

-- | Decode an instruction to determine the opcode and parameter modes.
--
-- >>> decode 1002
-- Just (Mul Abs Imm Abs)
decode :: Int -> Maybe (Opcode Mode)
decode n = traverse par =<< opcode
  where
    par i =
      case digit (i+1) n of
        0 -> Just Abs
        1 -> Just Imm
        2 -> Just Rel
        _ -> Nothing

    opcode =
      case n `mod` 100 of
        _ | n < 0 -> Nothing
        1  -> Just (Add 1 2 3)
        2  -> Just (Mul 1 2 3)
        3  -> Just (Inp 1)
        4  -> Just (Out 1)
        5  -> Just (Jnz 1 2)
        6  -> Just (Jz  1 2)
        7  -> Just (Lt  1 2 3)
        8  -> Just (Eq  1 2 3)
        9  -> Just (Arb 1)
        99 -> Just Hlt
        _  -> Nothing

-- | Arguments visited from left to right.
instance Traversable Opcode where
  {-# INLINE traverse #-}
  traverse f o =
    case o of
      Add x y z -> Add <$> f x <*> f y <*> f z
      Mul x y z -> Mul <$> f x <*> f y <*> f z
      Inp x     -> Inp <$> f x
      Out x     -> Out <$> f x
      Jnz x y   -> Jnz <$> f x <*> f y
      Jz  x y   -> Jz  <$> f x <*> f y
      Lt  x y z -> Lt  <$> f x <*> f y <*> f z
      Eq  x y z -> Eq  <$> f x <*> f y <*> f z
      Arb x     -> Arb <$> f x
      Hlt       -> pure Hlt

-- | Extract the ith digit from a number.
--
-- >>> digit 0 2468
-- 8
-- >>> digit 3 2468
-- 2
-- >>> digit 4 2468
-- 0
digit :: Int {- ^ position -} -> Int {- ^ number -} -> Int {- ^ digit -}
digit i x = x `quot` (10^i) `rem` 10
