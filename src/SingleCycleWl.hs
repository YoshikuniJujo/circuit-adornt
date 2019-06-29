{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module SingleCycleWl where

import Circuit
import Clock
import Memory
import Alu
import ControlWl
import SingleCycleParts

sampleCpuWl :: CircuitBuilder (
	Clock, ProgramCounter, RiscvInstMem,
	RiscvRegisterFile, RiscvAlu, RiscvDataMem )
sampleCpuWl = do
	(cl, pc, rim, npc, pcin) <- tryInstMem 70

	(mctrlin, mctrlout) <- control

	(rrf, alu, rdm) <- makeCpu cl pc rim npc pcin mctrlin mctrlout
	return (cl, pc, rim, rrf, alu, rdm)
