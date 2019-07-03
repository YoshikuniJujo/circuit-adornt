{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module PipelinedStall where

import Circuit
import Element
import Clock
import Memory
import ControlPla
import ControlParts
import ImmGen
import Alu

import DataHazardUnits

data IfId = IfId {
	ifIdProgramCounter :: RegisterWithWriteFlag,
	ifIdInstruction :: RegisterWithWriteFlag }
	deriving Show

instructionFetch :: CircuitBuilder
	(Clock, ProgramCounter, RiscvInstMem, IfId, IWire, IWire)
instructionFetch = do
	cl <- clock 30
--	cl <- clock 60
	pc <- programCounter
	pcClocked cl pc

	addr <- riscvAdder
	connectWire64 (pcOutput pc) (addrArgA addr)
	(pcSrc, pc', pcBr, pcout) <- mux2
	four <- constGate64 $ Bits 4
	connectWire64 four (addrArgB addr)
	connectWire64 (addrResult addr) pc'
	connectWire64 pcout (pcInput pc)

	ifIdPc <- registerWithWriteFlag
	connectWire0 (clockSignal cl) (rgwfClock ifIdPc)
	connectWire64 (pcOutput pc) (rgwfInput ifIdPc)
	rim <- riscvInstMem 64
	connectWire64 (pcOutput pc) (rimReadAddress rim)
	ifIdInst <- registerWithWriteFlag
	connectWire0 (clockSignal cl) (rgwfClock ifIdInst)
	connectWire64 (rimOutput rim) (rgwfInput ifIdInst)
	return (cl, pc, rim,
		IfId {	ifIdProgramCounter = ifIdPc,
			ifIdInstruction = ifIdInst },
		pcSrc, pcBr)

connectWriteIfId :: OWire -> IfId -> CircuitBuilder ()
connectWriteIfId wf ifId = do
	connectWire0 wf (rgwfWrite $ ifIdProgramCounter ifId)
	connectWire0 wf (rgwfWrite $ ifIdInstruction ifId)

data IdEx = IdEx {
	idExControl :: Register,
	idExProgramCounter :: Register,
	idExReadData1 :: Register,
	idExReadData2 :: Register,
	idExImmediate :: Register,
	idExInstruction30_14_12 :: Register,
	idExRegisterSource1 :: Register,
	idExRegisterSource2 :: Register,
	idExWriteRegister :: Register }
	deriving Show

instructionDecode :: CircuitBuilder
	(IWire, IWire, IWire, RiscvRegisterFile, IdEx, IWire, IWire, IWire, IWire)
instructionDecode = do
	(clin, clout) <- idGate0
	(pcin, pcout) <- idGate64
	(instin, instout) <- idGate64

	idExPc <- register
	connectWire0 clout (rgClock idExPc)
	connectWire64 pcout (rgInput idExPc)

	(cntrlin, cntrlout) <- controlPla'
	connectWire64 instout cntrlin
	idExCtrl <- register
	connectWire0 clout (rgClock idExCtrl)
	(stall, cntrlNotStall, zeroin, cntrlStallOut) <- mux2
	connectWire64 cntrlout cntrlNotStall
	zero <- constGate64 (Bits 0)
	connectWire64 zero zeroin
	connectWire64 cntrlStallOut (rgInput idExCtrl)

	rrf <- riscvRegisterFile

	(cl, ncl) <- notGate0
	(cla, clb, clo) <- andGate0
	connectWire0 clout cl
	connectWire0 ncl cla
	connectWire0 ncl clb
	delay cla 15
	connectWire0 clo (rrfClock rrf)

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

	idExRs1 <- register
	idExRs2 <- register
	(connectWire0 clout . rgClock) `mapM_` [idExRs1, idExRs2]
	connectWire (instout, 5, 15) (rgInput idExRs1, 5, 0)
	connectWire (instout, 5, 20) (rgInput idExRs2, 5, 0)

	return (clin, pcin, instin, rrf,
		IdEx {
			idExControl = idExCtrl, idExProgramCounter = idExPc,
			idExReadData1 = idExRd1, idExReadData2 = idExRd2,
			idExImmediate = idExImm,
			idExInstruction30_14_12 = idExInst30_14_12,
			idExRegisterSource1 = idExRs1, idExRegisterSource2 = idExRs2,
			idExWriteRegister = idExWr },
		rrfWrite rrf, rrfWriteAddress rrf, rrfInput rrf, stall)

data ExMem = ExMem {
	exMemControl :: Register,
	exMemProgramAddress :: Register,
	exMemZero :: Register,
	exMemAluResult :: Register,
	exMemReadData2 :: Register,
	exMemWriteRegister :: Register }
	deriving Show

execution :: CircuitBuilder (
	IWire, IWire, IWire, IWire, IWire, IWire, IWire, IWire, ExMem,
	IWire, IWire, IWire, IWire )
execution = do
	(clin, clout) <- idGate0
	(ctrlin, ctrlout) <- idGate64
	(immin, immout) <- idGate64

	exMemCtrl <- register
	connectWire0 clout (rgClock exMemCtrl)
	connectWire64 ctrlout (rgInput exMemCtrl)

	addr <- riscvAdder
	exMemPa <- register
	connectWire0 clout (rgClock exMemPa)
	connectWire64 immout (addrArgB addr)
	connectWire64 (addrResult addr) (rgInput exMemPa)

	alu <- riscvAlu
	(i30_14_12, aluOp, aluCon) <- aluControl'
	connectWire (ctrlout, 2, 6) (aluOp, 2, 0)
	connectWire64 aluCon (aluOpcode alu)

	(mwRsltin, mwRsltout) <- idGate64
	(emRsltin, emRsltout) <- idGate64

	(fa, aluAin, mwRslt, emRslt, aluAout) <- mux3
	connectWire64 mwRsltout mwRslt
	connectWire64 emRsltout emRslt
	connectWire64 aluAout (aluArgA alu)

	(fb, aluBin, mwRslt', emRslt', aluBout) <- mux3
	connectWire64 mwRsltout mwRslt'
	connectWire64 emRsltout emRslt'

	(as, rd2, imm, aluB) <- mux2
	connectWire (ctrlout, 1, 5) (as, 1, 0)
	connectWire64 aluBout rd2
	connectWire64 immout imm
	connectWire64 aluB (aluArgB alu)

	exMemZ <- register
	exMemAr <- register
	connectWire0 clout (rgClock exMemZ)
	connectWire0 clout (rgClock exMemAr)
	connectWire64 (aluZero alu) (rgInput exMemZ)
	connectWire64 (aluResult alu) (rgInput exMemAr)

	exMemRd2 <- register
	connectWire0 clout (rgClock exMemRd2)
	connectWire64 aluBout (rgInput exMemRd2)

	exMemWr <- register
	connectWire0 clout (rgClock exMemWr)

	return (clin, ctrlin, addrArgA addr,
		aluAin, aluBin, immin, i30_14_12, rgInput exMemWr,
		ExMem {	exMemControl = exMemCtrl,
			exMemProgramAddress = exMemPa,
			exMemZero = exMemZ,
			exMemAluResult = exMemAr,
			exMemReadData2 = exMemRd2,
			exMemWriteRegister = exMemWr },
		fa, fb, mwRsltin, emRsltin)

data MemWb = MemWb {
	memWbControl :: Register,
	memWbReadData :: Register,
	memWbAluResult :: Register,
	memWbWriteRegister :: Register }
	deriving Show

dataAccess :: IWire -> IWire -> CircuitBuilder (
	IWire, IWire, IWire, IWire, IWire, IWire, IWire, RiscvDataMem, MemWb )
dataAccess pcSrc pcBr = do
	(clin, clout) <- idGate0
	(ctrlin, ctrlout) <- idGate64
	(pain, paout) <- idGate64
	(arin, arout) <- idGate64

	memWbCtrl <- register
	connectWire0 clout (rgClock memWbCtrl)
	connectWire64 ctrlout (rgInput memWbCtrl)

	(aa, zero, ao) <- andGate0
	connectWire (ctrlout, 1, 4) (aa, 1, 0)
	connectWire0 ao pcSrc
	connectWire64 paout pcBr

	rdm <- riscvDataMem 64
	connectWire0 clout (rdmClock rdm)
	connectWire (ctrlout, 1, 3) (rdmRead rdm, 1, 0)
	connectWire (ctrlout, 1, 2) (rdmWrite rdm, 1, 0)
	connectWire64 arout (rdmAddress rdm)
	memWbRd <- register
	memWbAr <- register
	connectWire0 clout (rgClock memWbRd)
	connectWire0 clout (rgClock memWbAr)
	connectWire64 (rdmOutput rdm) (rgInput memWbRd)
	connectWire64 arout (rgInput memWbAr)

	memWbWr <- register
	connectWire0 clout (rgClock memWbWr)

	return (clin, ctrlin, pain,
		zero, arin, rdmInput rdm, rgInput memWbWr, rdm, MemWb {
			memWbControl = memWbCtrl,
			memWbReadData = memWbRd,
			memWbAluResult = memWbAr,
			memWbWriteRegister = memWbWr })

writeBack :: IWire -> IWire -> IWire -> CircuitBuilder (IWire, IWire, IWire, IWire, OWire)
writeBack rgwr wrrg wrdt = do
	(ctrlin, ctrlout) <- idGate64
	(rdin, rdout) <- idGate64
	(arin, arout) <- idGate64
	(wrin, wrout) <- idGate64

	connectWire (ctrlout, 1, 1) (rgwr, 1, 0)

	(mtr, ar, rd, wd) <- mux2
	connectWire (ctrlout, 1, 0) (mtr, 1, 0)
	connectWire64 arout ar
	connectWire64 rdout rd
	connectWire64 wd wrdt

	connectWire64 wrout wrrg

	return (ctrlin, rdin, arin, wrin, wd)

pipelined :: CircuitBuilder (
	Clock, ProgramCounter, RiscvInstMem, IfId,
	RiscvRegisterFile, IdEx, ExMem, RiscvDataMem, MemWb )
pipelined = do
	(cl, pc, rim, ifId, pcSrc, pcBr) <- instructionFetch
	(idCl, idPc, idInst, rrf, idEx, rgwr, wrrg, wrdt, stall) <- instructionDecode
	connectWire0 (clockSignal cl) idCl
	connectWire64 (rgwfOutput $ ifIdProgramCounter ifId) idPc
	connectWire64 (rgwfOutput $ ifIdInstruction ifId) idInst
	(exCl, exCtrl, exPc, rd1, rd2, imm, i30_14_12, wr, exMem,
		fain, fbin, mwRslt, emRslt) <- execution
	connectWire0 (clockSignal cl) exCl
	connectWire64 (rgOutput $ idExControl idEx) exCtrl
	connectWire64 (rgOutput $ idExProgramCounter idEx) exPc
	connectWire64 (rgOutput $ idExReadData1 idEx) rd1
	connectWire64 (rgOutput $ idExReadData2 idEx) rd2
	connectWire64 (rgOutput $ idExImmediate idEx) imm
	connectWire64 (rgOutput $ idExInstruction30_14_12 idEx) i30_14_12
	connectWire64 (rgOutput $ idExWriteRegister idEx) wr
	(daCl, daCtrl, daPc, daZero, daAr, daWd, daWr, rdm, memWb) <- dataAccess pcSrc pcBr
	connectWire0 (clockSignal cl) daCl
	connectWire64 (rgOutput $ exMemControl exMem) daCtrl
	connectWire64 (rgOutput $ exMemProgramAddress exMem) daPc
	connectWire64 (rgOutput $ exMemZero exMem) daZero
	connectWire64 (rgOutput $ exMemAluResult exMem) daAr
	connectWire64 (rgOutput $ exMemReadData2 exMem) daWd
	connectWire64 (rgOutput $ exMemWriteRegister exMem) daWr
	(wbCtrl, wbRd, wbAr, wbWr, wrDt) <- writeBack rgwr wrrg wrdt
	connectWire64 (rgOutput $ memWbControl memWb) wbCtrl
	connectWire64 (rgOutput $ memWbReadData memWb) wbRd
	connectWire64 (rgOutput $ memWbAluResult memWb) wbAr
	connectWire64 (rgOutput $ memWbWriteRegister memWb) wbWr

	(exMemCtrl, memWbCtrl, exMemRd, memWbRd, rs1, rs2, fa, fb) <- forwardingUnit
	connectWire64 (rgOutput $ exMemControl exMem) exMemCtrl
	connectWire64 (rgOutput $ memWbControl memWb) memWbCtrl
	connectWire64 (rgOutput $ exMemWriteRegister exMem) exMemRd
	connectWire64 (rgOutput $ memWbWriteRegister memWb) memWbRd
	connectWire64 (rgOutput $ idExRegisterSource1 idEx) rs1
	connectWire64 (rgOutput $ idExRegisterSource2 idEx) rs2

	connectWire64 fa fain
	connectWire64 fb fbin
	connectWire64 wrDt mwRslt
	connectWire64 (rgOutput $ exMemAluResult exMem) emRslt

	one <- constGate0 (Bits 1)
	connectWriteProgramCounter one pc
	connectWriteIfId one ifId
	zero <- constGate0 (Bits 0)
	connectWire0 zero stall

	return (cl, pc, rim, ifId, rrf, idEx, exMem, rdm, memWb)

resetPipelineRegisters :: IfId -> IdEx -> ExMem -> MemWb -> Circuit -> Circuit
resetPipelineRegisters ifId idEx exMem memWb cct = let
	cct1 = resetRegisters [
		idExProgramCounter idEx,
		idExControl idEx,
		idExReadData1 idEx,
		idExReadData2 idEx,
		idExImmediate idEx,
		idExInstruction30_14_12 idEx,
		idExWriteRegister idEx,
		exMemControl exMem,
		exMemProgramAddress exMem,
		exMemZero exMem,
		exMemAluResult exMem,
		exMemReadData2 exMem,
		exMemWriteRegister exMem,
		memWbControl memWb,
		memWbReadData memWb,
		memWbAluResult memWb,
		memWbWriteRegister memWb ] cct
	cct2 = resetRegisterWithWriteFlag (ifIdProgramCounter ifId) cct1
	cct3 = resetRegisterWithWriteFlag (ifIdInstruction ifId) cct2 in
	cct3
