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
