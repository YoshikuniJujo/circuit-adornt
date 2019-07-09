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

trySingleCycle :: IO (Clock, ProgramCounter, InstructionMemory, Circuit)
trySingleCycle = do
	((cl, pc, im), cct) <- makeCircuitRandomIO singleCycle
	let	cct0 = resetClock cl cct
		cct1 = foldr (uncurry $ storeInstructionMemory im) cct0 [
			(0, Bits 1234567890),
			(4, Bits 9876543210) ]
		cct2 = resetProgramCounter pc cct1
		cct3 = clockOn cl cct2
	return (cl, pc, im, cct3)

runProgramCounter :: ProgramCounter -> Circuit -> Int -> [Word64]
runProgramCounter pc cct n =
	take n $ bitsToWord . peekOWire (pcOutput pc) <$> iterate step cct

runInstructionMemory :: InstructionMemory -> Circuit -> Int -> [Word64]
runInstructionMemory im cct n =
	take n $ bitsToWord . peekOWire (imOutput im) <$> iterate step cct
