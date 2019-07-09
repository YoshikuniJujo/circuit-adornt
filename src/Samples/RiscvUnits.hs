{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Samples.RiscvUnits where

import Data.Word

import Circuit
import Element
import Samples.Memory

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

data InstructionMemory = InstructionMemory {
	imWrite :: IWire, imAddress :: IWire,
	imInput :: IWire, imOutput :: OWire,

	imSwitch :: IWire, imOuterWrite :: IWire,
	imOuterAddress :: IWire, imOuterInput :: IWire,
	imDebugOutputs :: [OWire] }

instructionMemory :: Word8 -> CircuitBuilder InstructionMemory
instructionMemory n = do
	sr <- sram n
	(addrin, addrout) <- idGate
	(oaddrin, oaddrout) <- idGate
	connectWire (addrout, 62, 2) (srAddress sr, 62, 0)
	connectWire (oaddrout, 62, 2) (srOuterAddress sr, 62, 0)
	return InstructionMemory {
		imWrite = srWrite sr, imAddress = addrin,
		imInput = srInput sr, imOutput = srOutput sr,

		imSwitch = srSwitch sr, imOuterWrite = srOuterWrite sr,
		imOuterAddress = oaddrin, imOuterInput = srOuterInput sr,
		imDebugOutputs = srDebugOutputs sr }

storeInstructionMemory :: InstructionMemory -> Word16 -> Bits -> Circuit -> Circuit
storeInstructionMemory im addr_ (Bits wdt) cct = let
	cct1 = (!! 15) . iterate step
		$ setMultBits [sw, we, ad, ip] [1, 0, waddr, wdt] cct
	cct2 = (!! 15) . iterate step $ setBits we (Bits 1) cct1
	cct3 = (!! 15) . iterate step $ setBits we (Bits 0) cct2
	cct4 = (!! 15) . iterate step $ setBits sw (Bits 0) cct3 in
	cct4
	where
	sw = imSwitch im; we = imOuterWrite im
	ad = imOuterAddress im; ip = imOuterInput im
	waddr = fromIntegral addr_
