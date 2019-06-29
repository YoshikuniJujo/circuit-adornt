{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module SingleCycleParts (tryInstMem, makeCpu) where

import Data.Word

import Circuit
import Element
import Clock
import Memory
import Alu
import ImmGen
import ControlMp

tryInstMem :: Word8 -> CircuitBuilder (Clock, ProgramCounter, RiscvInstMem, OWire, IWire)
tryInstMem n = do
	cl <- clock n
	pc <- programCounter
	pcClocked cl pc
	ad <- riscvAdder
	connectWire64 (pcOutput pc) (addrArgA ad)
	four <- constGate64 (Bits 4)
	connectWire64 four (addrArgB ad)
	rim <- riscvInstMem 128
	connectWire64 (pcOutput pc) (rimReadAddress rim)
	return (cl, pc, rim, addrResult ad, pcInput pc)

data ReadReg = ReadReg Word8 deriving Show
data WriteReg = WriteReg Word8 deriving Show

makeCpu :: Clock -> ProgramCounter -> RiscvInstMem ->
	OWire -> IWire -> IWire -> OWire ->
	CircuitBuilder (RiscvRegisterFile, RiscvAlu, RiscvDataMem)
makeCpu mcl pc rim npc pcin mctrlin mctrlout = do
	connectWire64 (rimOutput rim) mctrlin
	(acinst, acctrl, acout) <- aluControl
	connectWire64 (rimOutput rim) acinst
	connectWire64 mctrlout  acctrl
	rrf <- riscvRegisterFile
	connectWire
		(instructionMemoryOutput rim, 5, 15)
		(registerFileReadAddress1 rrf, 5, 0)
	connectWire
		(instructionMemoryOutput rim, 5, 20)
		(registerFileReadAddress2 rrf, 5, 0)
	alu <- riscvAlu
	connectWire64 acout (aluOpcode alu)
	connectWire64 (rrfOutput1 rrf) (aluArgA alu)
	(immin, immout) <- immGen
	connectWire64 (rimOutput rim) immin
	(srcSel, srcReg, srcImm, srcConn) <- mux2
	connectWire (mctrlout, 1, 7) (srcSel, 1, 0)
	connectWire64 (rrfOutput2 rrf) srcReg
	connectWire64 immout srcImm
	connectWire64 srcConn (aluArgB alu)
	connectWire0 (clockSignal mcl) (rrfClock rrf)
	connectWire (mctrlout, 1, 5) (rrfWrite rrf, 1, 0)
	connectWire
		(instructionMemoryOutput rim, 5, 7)
		(registerFileWriteAddress rrf, 5, 0)
	rdm <- riscvDataMem 128
	connectWire0 (clockSignal mcl) (rdmClock rdm)
	connectWire64 (aluResult alu) (rdmAddress rdm)
	connectWire64 (rrfOutput2 rrf) (rdmInput rdm)
	connectWire (mctrlout, 1, 3) (rdmWrite rdm, 1, 0)
	connectWire (mctrlout, 1, 4) (rdmRead rdm, 1, 0)
	(rwSel, rwAlu, rwMem, rwOut) <- mux2
	connectWire (mctrlout, 1, 6) (rwSel, 1, 0)
	connectWire64 (aluResult alu) rwAlu
	connectWire64 (rdmOutput rdm) rwMem
	connectWire64 rwOut (rrfInput rrf)
	addr <- riscvAdder
	(flb, zr, tkbr) <- andGate0
	connectWire (mctrlout, 1, 2) (flb, 1, 0)
	connectWire0 (aluZero alu) zr
	(dbr, npc', brpc, nwpc) <- mux2
	connectWire0 tkbr dbr
	connectWire64 npc npc'
	connectWire64 (pcOutput pc) (addrArgA addr)
	connectWire64 immout (addrArgB addr)
	connectWire64 (addrResult addr) brpc
	connectWire64 nwpc pcin
	return (rrf, alu, rdm)
