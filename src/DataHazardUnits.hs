{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module DataHazardUnits where

import Circuit
import Element

{-

input: exMemCtrl, memWbCtrl, exMemRd, memWbRd, idExRs1, idExRs2
output:	forwardA, forwardB

-}

forwardingUnit :: CircuitBuilder (IWire, IWire, IWire, IWire, IWire, IWire, OWire, OWire)
forwardingUnit = do
	(rs1in, rs1out) <- idGate64
	(rs2in, rs2out) <- idGate64
	(fain, faout) <- idGate64
	(fbin, fbout) <- idGate64
	(exMemCtrl, exMemRd, rs1, rs2, fa, fb) <- exHazard
	(memWbCtrl, memWbRd, rs1', rs2', fa', fb') <- memHazard
	connectWire64 rs1out `mapM_` [rs1, rs1']
	connectWire64 rs2out `mapM_` [rs2, rs2']
	connectWire (fa, 1, 1) (fain, 1, 1)
	connectWire (fb, 1, 1) (fbin, 1, 1)
	(oaa, oab, oao) <- andGate0
	(oba, obb, obo) <- andGate0
	connectWire0 fa' oaa
	connectWire0 fa oab
	connectWire0 oao fain
	connectWire0 fb' oba
	connectWire0 fb obb
	connectWire0 obo fbin
	return (exMemCtrl, memWbCtrl, exMemRd, memWbRd, rs1in, rs2in, faout, fbout)

{-

input:			idExRs1, idExRs2, exMemCtrl(1), exMemRd
output:			forwardA(1), forwardB(1)
negative output:	forwardA(0), forwardB(0)

-}

exHazard :: CircuitBuilder (IWire, IWire, IWire, IWire, OWire, OWire)
exHazard = do
	(cntrlin, cntrlout) <- idGate64
	(rdin, rdout) <- idGate64
	(fain, faout) <- idGate64
	(fbin, fbout) <- idGate64
	(cntrlA, rdA, rs1, rslt1) <- detectHazard1
	(cntrlB, rdB, rs2, rslt2) <- detectHazard1
	connectWire64 cntrlout cntrlA
	connectWire64 cntrlout cntrlB
	connectWire64 rdout rdA
	connectWire64 rdout rdB
	connectWire (rslt1, 1, 0) (fain, 1, 1)
	connectWire (rslt2, 1, 0) (fbin, 1, 1)
	(ain, na) <- notGate0
	(bin, nb) <- notGate0
	connectWire0 rslt1 ain
	connectWire0 rslt2 bin
	connectWire0 na fain
	connectWire0 nb fbin
	return (cntrlin, rdin, rs1, rs2, faout, fbout)

detectHazard1 :: CircuitBuilder (IWire, IWire, IWire, OWire)
detectHazard1 = do
	(cntrlin, cntrlout) <- idGate64
	(aa, ab, ao) <- andGate0
	(rd, rs, eq) <- equal5
	connectWire (cntrlout, 1, 1) (aa, 1, 0)
	connectWire0 eq ab
	return (cntrlin, rd, rs, ao)

{-

input:	idExRs1, idExRs2, MemWbCtrl(1), memCtrlRd
output:	forwardA(0), forwardB(0)

-}

memHazard :: CircuitBuilder (IWire, IWire, IWire, IWire, OWire, OWire)
memHazard = do
	(cntrlin, cntrlout) <- idGate64
	(rdin, rdout) <- idGate64
	(fain, faout) <- idGate64
	(fbin, fbout) <- idGate64
	(cntrlA, rdA, rs1, rslt1) <- detectHazard1
	(cntrlB, rdB, rs2, rslt2) <- detectHazard1
	connectWire64 cntrlout cntrlA
	connectWire64 cntrlout cntrlB
	connectWire64 rdout rdA
	connectWire64 rdout rdB
	connectWire0 rslt1 fain
	connectWire0 rslt2 fbin
	return (cntrlin, rdin, rs1, rs2, faout, fbout)

detectLoadHazard :: CircuitBuilder (IWire, IWire, IWire, IWire, OWire)
detectLoadHazard = do
	(idExMemRead, collisionin, hazard) <- andGate0
	(collision1, collision2, collisionout) <- orGate0
	connectWire0 collisionout collisionin
	(idExRdin, idExRdout) <- idGate64
	(idExRd, ifIdRs1, eq1) <- equal5
	(idExRd', ifIdRs2, eq2) <- equal5
	connectWire64 idExRdout idExRd
	connectWire64 idExRdout idExRd'
	connectWire0 eq1 collision1
	connectWire0 eq2 collision2
	return (idExMemRead, idExRdin, ifIdRs1, ifIdRs2, hazard)
