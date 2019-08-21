{-# LANGUAGE BinaryLiterals #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Samples.Alu where

import Circuit.Adornt.Parts
import Circuit.Adornt.Builder

alu :: CircuitBuilder (IWire, IWire, IWire, OWire, OWire, OWire)
alu = do
	(ain, aout) <- idGate
	(bin, bout) <- idGate
	(nain, naout) <- notGate
	(nbin, nbout) <- notGate
	connectWire64 aout nain
	connectWire64 bout nbin
	(ainv, main, mnain, maout) <- mux2
	(binv, mbin, mnbin, mbout) <- mux2
	connectWire64 aout main
	connectWire64 naout mnain
	connectWire64 bout mbin
	connectWire64 nbout mnbin

	(aa, ab, ao) <- andGate
	(oa, ob, oo) <- orGate
	(ci, ca, cb, cs, co) <- carries
	(xc, xa, xb, xo) <- xorGate3
	(op, a, o, s, slt, r) <- mux4
	connectWire64 maout `mapM_` [aa, oa, ca, xa]
	connectWire64 mbout `mapM_` [ab, ob, cb, xb]
	connectWire64 cs xc
	connectWire64 ao a
	connectWire64 oo o
	connectWire64 xo s

	cz <- constGate 0

	(ofci, ofco, ovfl) <- xorGate
	connectWire (cs, 1, 63) (ofci, 1, 0)
	connectWire (cz, 63, 0) (ofci, 63, 1)
	connectWire0 co ofco
	connectWire (cz, 63, 0) (ofco, 63, 1)

	(flb, ovfl', lt) <- xorGate
	connectWire (xo, 1, 63) (flb, 1, 0)
	connectWire0 ovfl ovfl'
	connectWire0 lt slt
	zero <- constGate 0
	connectWire (zero, 63, 1) (slt, 63, 1)

	(zin, zout) <- zeroDetector
	connectWire64 r zin

	(clin, clout) <- idGate
	connectWire (clout, 1, 3) (ainv, 1, 0)
	connectWire (clout, 1, 2) (binv, 1, 0)
	connectWire (clout, 1, 2) (ci, 1, 0)
	connectWire (clout, 2, 0) (op, 2, 0)
	return (clin, ain, bin, r, zout, ovfl)

adder :: CircuitBuilder (IWire, IWire, OWire)
adder = do
	(ctr, a, b, r, _z, _ovfl) <- alu
	ad <- constGate 0b0010
	connectWire64 ad ctr
	return (a, b, r)
