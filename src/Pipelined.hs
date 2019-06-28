{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Pipelined where

import Circuit
import Clock
import Memory
import Control
import Alu

data IfId = IfId {
	ifIdProgramCounter :: Register,
	ifIdInstruction :: Register }
	deriving Show

instructionFetch :: CircuitBuilder
	(Clock, ProgramCounter, RiscvInstMem, IfId, IWire, IWire)
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
		IfId {	ifIdProgramCounter = ifIdPc,
			ifIdInstruction = ifIdInst },
		undefined, undefined)

data IdEx = IdEx {
	idExControl :: Register,
	idExProgramCounter :: Register,
	idExReadData1 :: Register,
	idExReadData2 :: Register,
	idExImmediate :: Register,
	idExInst30_14_12 :: Register,
	idExWriteRegister :: Register }
	deriving Show

instructionDecode :: CircuitBuilder
	(IWire, IWire, IWire, RiscvRegisterFile, MainController, IdEx)
instructionDecode = do
	(clin, clout) <- idGate0
	(pcin, pcout) <- idGate64
	idExPc <- register
	connectWire0 clout (rgClock idExPc)
	connectWire64 pcout (rgInput idExPc)
	return (clin, pcin, undefined, undefined, undefined, IdEx {
		idExProgramCounter = idExPc
		})

pipelined :: CircuitBuilder (
	Clock, ProgramCounter, RiscvInstMem, IfId,
	RiscvRegisterFile, MainController, IdEx )
pipelined = do
	(cl, pc, rim, ifId, _, _) <- instructionFetch
	(idCl, idPc, idInst, rrf, mc, idEx) <- instructionDecode
	connectWire0 (clockSignal cl) idCl
	connectWire64 (rgOutput $ ifIdProgramCounter ifId) idPc
	return (cl, pc, rim, ifId, rrf, mc, idEx)

resetPipelineRegisters :: IfId -> IdEx -> Circuit -> Circuit
resetPipelineRegisters ifId idEx = resetRegisters [
	ifIdProgramCounter ifId,
	ifIdInstruction ifId,
	idExProgramCounter idEx ]
