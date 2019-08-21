module Samples.TryTools where

import System.Random

import Circuit.Adornt.Simulator
import Circuit.Adornt.Builder

makeCircuitRandom :: RandomGen g => g -> CircuitBuilder a -> (a, CircuitSimulator)
makeCircuitRandom g = prepareSimulator (wordToBits <$> randoms g)

makeCircuitRandomIO :: CircuitBuilder a -> IO (a, CircuitSimulator)
makeCircuitRandomIO cb = (`makeCircuitRandom` cb) <$> newStdGen
