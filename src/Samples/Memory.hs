{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Samples.Memory where

import Control.Monad
import Data.Word

import Circuit.Adornt.Simulator
import Circuit.Adornt.Parts
import Circuit.Adornt.Builder

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
	(addr, decout) <- decoder $ fromIntegral n
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

storeRegisterFile :: RegisterFileWithSwitch -> Word8 -> Bits -> CircuitSimulator -> CircuitSimulator
storeRegisterFile rfws adr_ d cct = let
	cct0 = (!! 5) . iterate step
		$ setBits (rfwsSwitch rfws) (wordToBits 1) cct
	cct1 = (!! 5) . iterate step
		. setBits (rfwsOuterWriteAddress rfws) adr
		$ setBits (rfwsOuterInput rfws) d cct0
	cct2 = (!! 10) . iterate step
		. setBits (rfwsOuterClock rfws) (wordToBits 1)
		$ setBits (rfwsOuterWrite rfws) (wordToBits 1) cct1
	cct3 = (!! 25) . iterate step
		. setBits (rfwsOuterClock rfws) (wordToBits 0)
		$ setBits (rfwsOuterWrite rfws) (wordToBits 0) cct2
	cct4 = (!! 5) . iterate step
		$ setBits (rfwsSwitch rfws) (wordToBits 0) cct3 in
	cct4
	where adr = wordToBits $ fromIntegral adr_

sramWrite :: Word8 -> CircuitBuilder (IWire, IWire, IWire, [OWire], [OWire])
sramWrite n = do
	(wein, weout) <- idGate
	(addr, decs) <- decoder $ fromIntegral n
	(dtin, dtout) <- idGate
	(decins, weins, cout) <- unzip3 <$> fromIntegral n `replicateM` andGate
	(cs, ds, qs, _nqs) <- unzip4 <$> fromIntegral n `replicateM` dlatch
	zipWithM_ connectWire0 decs decins
	connectWire0 weout `mapM_` weins
	zipWithM_ connectWire0 cout cs
	connectWire64 dtout `mapM_` ds
	return (wein, addr, dtin, decs, qs)

sramReadUnit :: Word8 -> [OWire] -> [OWire] -> CircuitBuilder OWire
sramReadUnit n decs os = do
	(tas, tbs, tos) <- unzip3 <$> fromIntegral n `replicateM` triGate
	zipWithM_ connectWire64 os tas
	zipWithM_ connectWire0 decs tbs
	(oin, oout) <- idGate
	flip connectWire64 oin `mapM_` tos
	return oout

sramGen :: Word8 -> CircuitBuilder (IWire, IWire, IWire, OWire, [OWire])
sramGen n = do
	(we, addr, dt, decs, qs) <- sramWrite n
	out <- sramReadUnit n decs qs
	return (we, addr, dt, out, qs)

data Sram = Sram {
	srWrite :: IWire, srAddress :: IWire,
	srInput :: IWire, srOutput :: OWire,

	srSwitch :: IWire, srOuterWrite :: IWire,
	srOuterAddress :: IWire, srOuterInput :: IWire,
	srDebugOutputs :: [OWire] }
	deriving Show

sram :: Word8 -> CircuitBuilder Sram
sram n = do
	(we0, addr0, dt0, out, qs) <- sramGen n
	(swin, swout) <- idGate
	(sw0, we, we', weout) <- mux2
	(sw1, addr, addr', addrout) <- mux2
	(sw2, dt, dt', dtout) <- mux2
	connectWire0 swout `mapM_` [sw0, sw1, sw2]
	connectWire0 weout we0
	connectWire64 addrout addr0
	connectWire64 dtout dt0
	return $ Sram {
		srWrite = we, srAddress = addr,
		srInput = dt, srOutput = out,

		srSwitch = swin,
		srOuterWrite = we', srOuterAddress = addr',
		srOuterInput = dt',
		srDebugOutputs = qs }

storeSram :: Sram -> Word8 -> Bits -> CircuitSimulator -> CircuitSimulator
storeSram sr addr_ wdt_ cct = let
	cct1 = (!! 15) . iterate step
		$ setMultiBits [sw, we, ad, ip] [1, 0, waddr, wdt] cct
	cct2 = (!! 15) . iterate step $ setBits we (wordToBits 1) cct1
	cct3 = (!! 15) . iterate step $ setBits we (wordToBits 0) cct2
	cct4 = (!! 15) . iterate step $ setBits sw (wordToBits 0) cct3 in
	cct4
	where
	sw = srSwitch sr; we = srOuterWrite sr
	ad = srOuterAddress sr; ip = srOuterInput sr
	waddr = fromIntegral addr_
	wdt = bitsToWord wdt_
