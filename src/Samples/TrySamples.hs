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
import Samples.AluController

import Samples.SingleCycle
import Instructions.SampleInstructions

{-
sampleLoadInst, sampleStoreInst, sampleSubInst, sampleAddInst, sampleBeqInst :: Word64
sampleLoadInst = encodeInst $ Load (Reg 10) 56 (Reg 15)
sampleStoreInst = encodeInst $ Store (Reg 1) 8 (Reg 2)
sampleSubInst = encodeInst $ Sub (Reg 30) (Reg 1) (Reg 2)
sampleAddInst = encodeInst $ Add (Reg 15) (Reg 10) (Reg 15)
sampleBeqInst = encodeInst $ Beq (Reg 30) (Reg 31) 20

sampleInstControlInstructions :: [Word64]
sampleInstControlInstructions = [
	sampleLoadInst, sampleStoreInst,
	sampleSubInst, sampleAddInst, sampleBeqInst ] ++ (encodeInst <$> [
	Nop, Nop, Nop, Nop, Add (Reg 3) (Reg 1) (Reg 10) ])
	-}

trySingleCycle :: IO (
	Clock, ProgramCounter, InstructionMemory, RegisterFileWithSwitch,
	OWire, OWire, OWire, (OWire, OWire, OWire), Circuit )
trySingleCycle = do
	((cl, pc, im, rf, ctrl, aluctrl, imm, aluout), cct) <- makeCircuitRandomIO singleCycle
	let	cct0 = resetClock cl cct
		cct1 = foldr (uncurry $ storeInstructionMemory im) cct0
			$ zip [0, 4 .. ] (Bits <$> sampleInstControlInstructions)
		cct2 = foldr (uncurry $ storeRegisterFile rf) cct1
			$ zip [15, 2, 1, 10, 30, 31] [
				Bits 1234567890,
				Bits 9876543210,
				Bits 9999999999,
				Bits 1111111111,
				Bits 7777777777,
				Bits 3333333333 ]
		cct3 = resetProgramCounter pc cct2
		cct4 = clockOn cl cct3
	return (cl, pc, im, rf, ctrl, aluctrl, imm, aluout, cct4)

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
