{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Samples.Memory where

import Circuit
import Element
import Samples.Clock

srlatch :: CircuitBuilder Wire22
srlatch = do
	(r, nqin, qout) <- norGate
	(s, qin, nqout) <- norGate
	connectWire64 nqout nqin
	connectWire64 qout qin
	return (r, s, qout, nqout)

dlatch :: CircuitBuilder Wire22
dlatch = do
	(cin, cout) <- idGate
	(din, dout) <- idGate
	(cr, dr, rout) <- andNotBGate
	(cs, ds, sout) <- andGate
	(r, s, q, nq) <- srlatch
	connectWire0_64 cout `mapM_` [cr, cs]
	connectWire64 dout `mapM_` [dr, ds]
	connectWire64 rout r
	connectWire64 sout s
	return (cin, din, q, nq)

testDlatch :: CircuitBuilder (Clock, IWire, OWire, OWire)
testDlatch = do
	cl <- clock 15
	(c, d, q, nq) <- dlatch
	connectWire0 (clockSignal cl) c
	return (cl, d, q, nq)

dflipflop :: CircuitBuilder Wire22
dflipflop = do
	(cin, cout) <- idGate
	(ni, no) <- notGate
	connectWire0 cout ni
	(cm, dm, qm, _nqm) <- dlatch
	(cs, ds, qs, nqs) <- dlatch
	connectWire0 cout cm
	connectWire0 no cs
	connectWire64 qm ds
	return (cin, dm, qs, nqs)

testDflipflop :: CircuitBuilder(Clock, IWire, OWire, OWire)
testDflipflop = do
	cl <- clock 15
	(c, d, q, nq) <- dflipflop
	connectWire0 (clockSignal cl) c
	return (cl, d, q, nq)
