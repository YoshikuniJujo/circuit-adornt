module Samples.TryTools where

import System.Random

import Circuit

makeCircuitRandom :: RandomGen g => g -> CircuitBuilder a -> (a, Circuit)
makeCircuitRandom g = makeCircuit (Bits <$> randoms g)

makeCircuitRandomIO :: CircuitBuilder a -> IO (a, Circuit)
makeCircuitRandomIO cb = (`makeCircuitRandom` cb) <$> newStdGen
