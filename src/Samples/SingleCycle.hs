{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Samples.SingleCycle where

import Circuit.Adornt.Parts
import Circuit.Adornt.Builder

import Samples.RiscvUnits
import Samples.Clock
import Samples.Alu
import Samples.Memory
import Samples.ControlPla
import Samples.AluController
import Samples.ImmGen

singleCycle :: CircuitBuilder (
	Clock, ProgramCounter, InstructionMemory, RegisterFileWithSwitch,
	OWire, OWire, OWire, (OWire, OWire, OWire) )
singleCycle = do
	cl <- clock 35
	pc <- programCounter
	(a, b, o) <- adder
	four <- constGate 4
	connectWire0 (clockSignal cl) (pcClock pc)
	connectWire64 (pcOutput pc) a
	connectWire64 four b
	connectWire64 o (pcInput pc)

	im <- instructionMemory 64
	connectWire64 (pcOutput pc) (imAddress im)

	rf <- registerFileWithSwitch 32
	connectWire (imOutput im, 5, 15)
		(rfReadAddress1 $ rfwsRegisterFile rf, 5, 0)
	connectWire (imOutput im, 5, 20)
		(rfReadAddress2 $ rfwsRegisterFile rf, 5, 0)

	(ctrlin, ctrlout) <- controlPla
	connectWire64 (imOutput im) ctrlin

	(ctrlalu, instalu, aluctrl) <- aluController
	connectWire64 ctrlout ctrlalu
	connectWire64 (imOutput im) instalu

	(immin, immout) <- immGen
	connectWire64 (imOutput im) immin

	(actrl, arga, argb, arslt, azero, aovfl) <- alu
	(ctrlas, rfout2, imm, argbout) <- mux2
	connectWire64 aluctrl actrl
	connectWire64 (rfOutput1 $ rfwsRegisterFile rf) arga
	connectWire (ctrlout, 1, 5) (ctrlas, 1, 0)
	connectWire64 (rfOutput2 $ rfwsRegisterFile rf) rfout2
	connectWire64 immout imm
	connectWire64 argbout argb

	return (cl, pc, im, rf, ctrlout, aluctrl, immout, (arslt, azero, aovfl))
