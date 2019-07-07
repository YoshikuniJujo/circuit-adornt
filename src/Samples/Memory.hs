{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Samples.Memory where

import Control.Monad
import Data.Word

import Circuit
import Element
import Samples.Clock
import Tools

testDlatch :: CircuitBuilder (Clock, IWire, OWire, OWire)
testDlatch = do
	cl <- clock 15
	(c, d, q, nq) <- dlatch
	connectWire0 (clockSignal cl) c
	return (cl, d, q, nq)

testDflipflop :: CircuitBuilder(Clock, IWire, OWire, OWire)
testDflipflop = do
	cl <- clock 15
	(c, d, q, nq) <- dflipflop
	connectWire0 (clockSignal cl) c
	return (cl, d, q, nq)

registerFileWrite :: Word8 -> CircuitBuilder (IWire, IWire, IWire, IWire, [OWire])
registerFileWrite n = do
	(cl, ww, cwout) <- andGate
	(addr, decout) <- decoder' $ fromIntegral n
	(din, dout) <- idGate
	(cws, decin, cout) <- unzip3 <$> fromIntegral n `replicateM` andGate
	(cs, ds, qs, _nqs) <- unzip4 <$> fromIntegral n `replicateM` dflipflop
	connectWire0 cwout `mapM_` cws
	zipWithM_ connectWire0 decout decin
	zipWithM_ connectWire0 cout cs
	connectWire64 dout `mapM_` ds
	return (cl, ww, addr, din, qs)

testRegisterFileWrite :: CircuitBuilder (Clock, IWire, IWire, IWire, [OWire])
testRegisterFileWrite = do
	cl <- clock 15
	(c, wr, addr, dt, qs) <- registerFileWrite 8
	connectWire0 (clockSignal cl) c
	return (cl, wr, addr, dt, qs)

registerFileReadUnit :: Word8 -> [OWire] -> CircuitBuilder (IWire, OWire)
registerFileReadUnit n os = do
	(addr, ds, r) <- multiplexer $ fromIntegral n
	zipWithM_ connectWire64 os ds
	return (addr, r)

data RegisterFile = RegisterFile {
	rfClock :: IWire,
	rfReadAddress1 :: IWire, rfReadAddress2 :: IWire,
	rfWrite :: IWire, rfWriteAddress :: IWire, rfInput :: IWire,
	rfOutput1 :: OWire, rfOutput2 :: OWire, rfDebugOutputs :: [OWire] }
	deriving Show

registerFile :: Word8 -> CircuitBuilder RegisterFile
registerFile n = do
	(c, w, waddr, d, os) <- registerFileWrite n
	(raddr1, r1) <- registerFileReadUnit n os
	(raddr2, r2) <- registerFileReadUnit n os
	return RegisterFile {
		rfClock = c,
		rfReadAddress1 = raddr1, rfReadAddress2 = raddr2,
		rfWrite = w, rfWriteAddress = waddr, rfInput = d,
		rfOutput1 = r1, rfOutput2 = r2, rfDebugOutputs = os }

data RegisterFileWithSwitch = RegisterFileWithSwitch {
	rfwsRegisterFile :: RegisterFile,
	rfwsSwitch :: IWire,
	rfwsOuterClock :: IWire,
	rfwsOuterWrite :: IWire,
	rfwsOuterInput :: IWire,
	rfwsOuterWriteAddress :: IWire }
	deriving Show

registerFileWithSwitch :: Word8 -> CircuitBuilder RegisterFileWithSwitch
registerFileWithSwitch n = do
	rf <- registerFile n
	(swin, swout) <- idGate
	(sw0, c, c', co) <- mux2
	(sw1, w, w', wo) <- mux2
	(sw2, wa, wa', wao) <- mux2
	(sw3, d, d', dto) <- mux2
	connectWire0 swout `mapM_` [sw0, sw1, sw2, sw3]
	connectWire0 co (rfClock rf)
	connectWire0 wo (rfWrite rf)
	connectWire64 wao (rfWriteAddress rf)
	connectWire64 dto (rfInput rf)
	return RegisterFileWithSwitch {
		rfwsRegisterFile = rf {
			rfClock = c,
			rfWrite = w, rfWriteAddress = wa, rfInput = d },
		rfwsSwitch = swin,
		rfwsOuterClock = c',
		rfwsOuterWrite = w',
		rfwsOuterWriteAddress = wa', rfwsOuterInput = d' }

storeRegisterFile :: RegisterFileWithSwitch -> Word8 -> Bits -> Circuit -> Circuit
storeRegisterFile rfws adr_ d cct = let
	cct0 = (!! 5) . iterate step
		$ setBits (rfwsSwitch rfws) (Bits 1) cct
	cct1 = (!! 5) . iterate step
		. setBits (rfwsOuterWriteAddress rfws) adr
		$ setBits (rfwsOuterInput rfws) d cct0
	cct2 = (!! 10) . iterate step
		. setBits (rfwsOuterClock rfws) (Bits 1)
		$ setBits (rfwsOuterWrite rfws) (Bits 1) cct1
	cct3 = (!! 25) . iterate step
		. setBits (rfwsOuterClock rfws) (Bits 0)
		$ setBits (rfwsOuterWrite rfws) (Bits 0) cct2
	cct4 = (!! 5) . iterate step
		$ setBits (rfwsSwitch rfws) (Bits 0) cct3 in
	cct4
	where adr = Bits $ fromIntegral adr_
