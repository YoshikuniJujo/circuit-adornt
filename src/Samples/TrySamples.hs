{-# OPTIONS_GHC -fno-warn-tabs #-}

module Samples.TrySamples () where

import Data.Word

import Circuit
import Samples.TryTools
import Samples.Simple
import Samples.Alu
import Samples.Clock
import Samples.Memory
import Samples.RiscvUnits

import Samples.SingleCycle

trySingleCycle :: IO (Clock, ProgramCounter, Circuit)
trySingleCycle = do
	((cl, pc), cct) <- makeCircuitRandomIO singleCycle
	let	cct0 = resetClock cl cct
		cct1 = resetProgramCounter pc cct0
		cct2 = clockOn cl cct1
	return (cl, pc, cct2)

runProgramCounter :: ProgramCounter -> Circuit -> Int -> [Word64]
runProgramCounter pc cct n =
	take n $ bitsToWord . peekOWire (pcOutput pc) <$> iterate step cct
