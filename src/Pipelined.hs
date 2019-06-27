{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Pipelined where

import Circuit
import Clock
import Memory
import Alu

data IfId = IfId {
	ifIdProgramCounter :: Register,
	ifIdInstruction :: Register }
	deriving Show

instructionFetch :: CircuitBuilder (Clock, ProgramCounter, RiscvInstMem, IfId, IWire, IWire)
instructionFetch = do
	cl <- clock 30
	pc <- programCounter
	pcClocked cl pc
	addr <- riscvAdder
	connectWire64 (pcOutput pc) (addrArgA addr)
	four <- constGate64 $ Bits 4
	connectWire64 four (addrArgB addr)
	connectWire64 (addrResult addr) (pcInput pc)
	ifIdPc <- register
	connectWire0 (clockSignal cl) (rgClock ifIdPc)
	connectWire64 (pcOutput pc) (rgInput ifIdPc)
	rim <- riscvInstMem 64
	connectWire64 (pcOutput pc) (rimReadAddress rim)
	ifIdInst <- register
	connectWire0 (clockSignal cl) (rgClock ifIdInst)
	connectWire64 (rimOutput rim) (rgInput ifIdInst)
	return (cl, pc, rim,
		IfId { ifIdProgramCounter = ifIdPc, ifIdInstruction = ifIdInst },
		undefined, undefined)
