{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Samples.Alu where

import Circuit
import Element
import Samples.CarryLookahead2

alu :: CircuitBuilder (IWire, IWire, IWire, IWire, IWire, IWire, OWire, OWire)
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
	(op, a, o, s, r) <- mux3
	connectWire64 maout `mapM_` [aa, oa, ca, xa]
	connectWire64 mbout `mapM_` [ab, ob, cb, xb]
	connectWire64 cs xc
	connectWire64 ao a
	connectWire64 oo o
	connectWire64 xo s

	(ofci, ofco, ovfl) <- xorGate
	connectWire (cs, 1, 63) (ofci, 1, 0)
	connectWire0 co ofco
	return (ainv, binv, op, ci, ain, bin, r, ovfl)
