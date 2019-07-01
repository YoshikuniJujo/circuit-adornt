{-# OPTIONS_GHC -fno-warn-tabs #-}

module TryPipelined () where

import Circuit
import Clock
import Memory
import ControlMp
import Pipelined
import SampleInstructions

((cl, pc, rim, ifId, rrf, idEx, exMem, ar), cct) = makeCircuit pipelined

cct0 = resetProgramCounter pc cct

cct1 = resetPipelineRegisters ifId idEx exMem cct0

cct2 = foldr (uncurry $ storeRiscvInstMem rim) cct1
	$ zip [0, 4 ..] sampleInstControlInstructions

{-

ld x10, 56(x15)		x15	?		(16, ?)
sd x1, 8(x2)		x2	x1		(40, 1234567890)
sub x30, x1, x2		x1	x2		(1234567890, 9876543210)
add x15, x10, x15	x10	x15		(9999999999, 1111111111)
beq x30, x31, 20	x30	x31		(1234567850, 1234567850)
nop
nop
nop
nop
add x3, x1, x10		x1: 1234567890	x10: 9876543210	x3: 11111111100

-}

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

cct3 = foldr (uncurry $ storeRiscvRegisterFile rrf) cct2 $ zip
	[1, 2, 10, 15, 30, 31] [
		1234567890,
		40,
		9999999999,
		16,
		7777777777,
		1234567850 ]

cct4 = resetProgramCounter pc cct3

cct5 = clockOn cl cct4
