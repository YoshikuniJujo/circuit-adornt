{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module SingleCyclePla where

import Circuit
import Clock
import Memory
import Alu
import ControlPla
import SingleCycleParts

sampleCpuPla :: CircuitBuilder (
	Clock, ProgramCounter, RiscvInstMem,
	RiscvRegisterFile, RiscvAlu, RiscvDataMem )
sampleCpuPla = do
	(cl, pc, rim, npc, pcin) <- tryInstMem 51

	(mctrlin, mctrlout) <- controlPla

	(rrf, alu, rdm) <- makeCpu cl pc rim npc pcin mctrlin mctrlout
	return (cl, pc, rim, rrf, alu, rdm)
