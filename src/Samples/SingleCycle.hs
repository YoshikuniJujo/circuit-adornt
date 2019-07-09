{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Samples.SingleCycle where

import Circuit
import Samples.RiscvUnits
import Samples.Clock
import Samples.Alu
import Samples.Memory

singleCycle :: CircuitBuilder
	(Clock, ProgramCounter, InstructionMemory, RegisterFileWithSwitch)
singleCycle = do
	cl <- clock 30
	pc <- programCounter
	(a, b, o) <- adder
	four <- constGate $ Bits 4
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

	return (cl, pc, im, rf)
