{-# OPTIONS_GHC -fno-warn-tabs #-}

module SimpleSamples () where

import Circuit

xorGate :: CircuitBuilder (IWire, IWire, OWire)
xorGate = do
	(ain, aout) <- idGate
	(bin, bout) <- idGate
	(aa, ab, ao) <- andGate
	(oa, ob, oo) <- orGate
	connectWire64 aout aa
	connectWire64 bout ab
	connectWire64 aout oa
	connectWire64 bout ob
	(ni, no) <- notGate
	(na, o, r) <- andGate
	connectWire64 ao ni
	connectWire64 no na
	connectWire64 oo o
	return (ain, bin, r)

sampleTri :: CircuitBuilder (IWire, IWire, IWire, IWire, OWire)
sampleTri = do
	(a1, b1, o1) <- triGate
	(a2, b2, o2) <- triGate
	(oin, oout) <- idGate
	connectWire64 o1 oin
	connectWire64 o2 oin
	return (a1, b1, a2, b2, oout)
