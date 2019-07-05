{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Samples.Alu where

import Circuit
import Element
import Samples.CarryLookahead2

alu :: CircuitBuilder (IWire, IWire, IWire, IWire, OWire)
alu = do
	(ain, aout) <- idGate
	(bin, bout) <- idGate
	(aa, ab, ao) <- andGate
	(oa, ob, oo) <- orGate
	(ci, ca, cb, cs, _co) <- carries
	(xc, xa, xb, xo) <- xorGate3
	(op, a, o, s, r) <- mux3
	connectWire64 aout `mapM_` [aa, oa, ca, xa]
	connectWire64 bout `mapM_` [ab, ob, cb, xb]
	connectWire64 cs xc
	connectWire64 ao a
	connectWire64 oo o
	connectWire64 xo s
	return (op, ci, ain, bin, r)
