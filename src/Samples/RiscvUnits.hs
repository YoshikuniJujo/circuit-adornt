{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Samples.RiscvUnits where

import Circuit
import Element
-- import Samples.Memory

data ProgramCounter = ProgramCounter {
	pcClock :: IWire, pcInput :: IWire, pcOutput :: OWire,

	pcSwitch :: IWire, pcOuterClock :: IWire, pcOuterInput :: IWire }
	deriving Show

programCounter :: CircuitBuilder ProgramCounter
programCounter = do
	(c, d, q, _nq) <- dflipflop

	(swin, swout) <- idGate
	(sw, ic, oc, cout) <- mux2
	(sw', idt, odt, dout) <- mux2
	connectWire0 swout `mapM_` [sw, sw']
	connectWire64 cout c
	connectWire64 dout d

	return ProgramCounter {
		pcClock = ic, pcInput = idt, pcOutput = q,
		pcSwitch = swin, pcOuterClock = oc, pcOuterInput = odt }

resetProgramCounter :: ProgramCounter -> Circuit -> Circuit
resetProgramCounter pc cct = let
	cct1 = (!! 20) . iterate step
		. setBits sw (Bits 1)
		. setBits cl (Bits 1) $ setBits dt (Bits 0) cct
	cct2 = (!! 10) . iterate step $ setBits cl (Bits 0) cct1 in
	setBits sw (Bits 0) cct2
	where
	sw = pcSwitch pc
	cl = pcOuterClock pc
	dt = pcOuterInput pc
