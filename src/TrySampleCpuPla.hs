{-# OPTIOnS_GHC -fno-warn-tabs #-}

module TrySampleCpuPla (trySingleCyclePlaCct, trySingleCyclePlaRrf) where

import Circuit
import Clock
import Memory
import ControlPla
import Alu
import SingleCyclePla
import SampleInstructions

((cl, pc, rim, rrf, alu, rdm), cct) = makeCircuit sampleCpuPla

cct1 = foldr (uncurry $ storeRiscvInstMem rim) cct
	$ zip [0, 4 ..] sampleInstControlInstructions

--	ld x10, 56(x15)		x15	?		(16, ?)
--	sd x1, 8(x2)		x2	x1		(40, 1234567890)
--	sub x30, x1, x2		x1	x2		(1234567890, 9876543210)
--	add x15, x10, x15	x10	x15		(9999999999, 1111111111)
--	beq x30, x31, 20	x30	x31		(1234567850, 1234567850)
--	nop
--	nop
--	nop
--	nop
--	add x3, x1, x10		x1: 1234567890	x10: 9876543210	x3: 11111111100

cct2 = foldr (uncurry $ storeRiscvRegisterFile rrf) cct1 $ zip
	[1, 2, 10, 15, 30, 31] [
		1234567890,	-- 1
		40,		-- 2
		9999999999,	-- 10
		16,		-- 15
		7777777777,	-- 30
		1234567850 ]	-- 31

cct3 = foldr (uncurry $ storeRiscvDataMem rdm) cct2 $ zip [0, 72] [
	5555555555, 9876543210 ]

cct4 = clockOn cl cct3

cct5 = resetProgramCounter pc cct4

-- cct6 = stopProgramCounter pc rrf 384 cct5

trySingleCyclePlaCct = cct5

trySingleCyclePlaRrf = rrf
