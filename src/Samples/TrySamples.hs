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
import Samples.ControlPla

import Samples.SingleCycle
import Instructions.SampleInstructions

trySingleCycle :: IO (
	Clock, ProgramCounter, InstructionMemory, RegisterFileWithSwitch,
	Circuit )
trySingleCycle = do
	((cl, pc, im, rf), cct) <- makeCircuitRandomIO singleCycle
	let	cct0 = resetClock cl cct
		cct1 = foldr (uncurry $ storeInstructionMemory im) cct0 $
			zip [0, 4 .. ] (Bits <$> sampleInstControlInstructions)
		cct2 = storeRegisterFile rf 15 (Bits 1234567890) cct1
		cct3 = resetProgramCounter pc cct2
		cct4 = clockOn cl cct3
	return (cl, pc, im, rf, cct4)

runProgramCounter :: ProgramCounter -> Circuit -> Int -> [Word64]
runProgramCounter pc cct n =
	take n $ bitsToWord . peekOWire (pcOutput pc) <$> iterate step cct

runInstructionMemory :: InstructionMemory -> Circuit -> Int -> [Word64]
runInstructionMemory im cct n =
	take n $ bitsToWord . peekOWire (imOutput im) <$> iterate step cct

runRegisterFile :: RegisterFileWithSwitch -> Circuit -> Int -> [(Word64, Word64)]
runRegisterFile rf cct n =
	take n $ (\[a, b] -> (a, b)) . peekMultOWires [
		rfOutput1 $ rfwsRegisterFile rf,
		rfOutput2 $ rfwsRegisterFile rf ] <$> iterate step cct
