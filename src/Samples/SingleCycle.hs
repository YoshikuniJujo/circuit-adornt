{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Samples.SingleCycle where

import Circuit
import Samples.RiscvUnits
import Samples.Clock
import Samples.Alu

singleCycle :: CircuitBuilder (Clock, ProgramCounter, InstructionMemory)
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

	return (cl, pc, im)
