{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Pipelined where

import Circuit
import Clock
import Memory
import ControlPla
import ImmGen
import Alu

data IfId = IfId {
	ifIdProgramCounter :: Register,
	ifIdInstruction :: Register }
	deriving Show

instructionFetch :: CircuitBuilder
	(Clock, ProgramCounter, RiscvInstMem, IfId, IWire, IWire)
instructionFetch = do
	cl <- clock 16
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
	idExInstruction30_14_12 :: Register,
	idExWriteRegister :: Register }
	deriving Show

instructionDecode :: CircuitBuilder
	(IWire, IWire, IWire, RiscvRegisterFile, IdEx)
instructionDecode = do
	(clin, clout) <- idGate0
	(pcin, pcout) <- idGate64
	(instin, instout) <- idGate64

	idExPc <- register
	connectWire0 clout (rgClock idExPc)
	connectWire64 pcout (rgInput idExPc)

	(cntrlin, cntrlout) <- controlPla
	connectWire64 instout cntrlin
	idExCtrl <- register
	connectWire0 clout (rgClock idExCtrl)
	connectWire64 cntrlout (rgInput idExCtrl)

	rrf <- riscvRegisterFile
	connectWire0 clout (rrfClock rrf)
	connectWire (instout, 5, 15) (registerFileReadAddress1 rrf, 5, 0)
	connectWire (instout, 5, 20) (registerFileReadAddress2 rrf, 5, 0)
	idExRd1 <- register
	idExRd2 <- register
	connectWire0 clout (rgClock idExRd1)
	connectWire0 clout (rgClock idExRd2)
	connectWire64 (rrfOutput1 rrf) (rgInput idExRd1)
	connectWire64 (rrfOutput2 rrf) (rgInput idExRd2)

	(immin, immout) <- immGen
	connectWire64 instout immin
	idExImm <- register
	connectWire0 clout (rgClock idExImm)
	connectWire64 immout (rgInput idExImm)

	idExInst30_14_12 <- register
	connectWire0 clout (rgClock idExInst30_14_12)
	connectWire (instout, 3, 12) (rgInput idExInst30_14_12, 3, 0)
	connectWire (instout, 1, 30) (rgInput idExInst30_14_12, 1, 3)

	idExWr <- register
	connectWire0 clout (rgClock idExWr)
	connectWire (instout, 5, 7) (rgInput idExWr, 5, 0)

	return (clin, pcin, instin, rrf, IdEx {
		idExControl = idExCtrl, idExProgramCounter = idExPc,
		idExReadData1 = idExRd1, idExReadData2 = idExRd2,
		idExImmediate = idExImm,
		idExInstruction30_14_12 = idExInst30_14_12,
		idExWriteRegister = idExWr })

pipelined :: CircuitBuilder
	(Clock, ProgramCounter, RiscvInstMem, IfId, RiscvRegisterFile, IdEx )
pipelined = do
	(cl, pc, rim, ifId, _, _) <- instructionFetch
	(idCl, idPc, idInst, rrf, idEx) <- instructionDecode
	connectWire0 (clockSignal cl) idCl
	connectWire64 (rgOutput $ ifIdProgramCounter ifId) idPc
	connectWire64 (rgOutput $ ifIdInstruction ifId) idInst
	return (cl, pc, rim, ifId, rrf, idEx)

resetPipelineRegisters :: IfId -> IdEx -> Circuit -> Circuit
resetPipelineRegisters ifId idEx = resetRegisters [
	ifIdProgramCounter ifId,
	ifIdInstruction ifId,
	idExProgramCounter idEx,
	idExControl idEx,
	idExReadData1 idEx,
	idExReadData2 idEx,
	idExImmediate idEx,
	idExInstruction30_14_12 idEx,
	idExWriteRegister idEx ]
