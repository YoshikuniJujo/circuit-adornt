{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Samples.AluController where

import Circuit.Adornt.Builder
import Circuit.Adornt.Parts

aluController :: CircuitBuilder (IWire, IWire, OWire)
aluController = do
	(cin, cout) <- idGate
	(iin, iout) <- idGate
	(oin, oout) <- idGate

	(p0, p1, p0p1) <- orGate
	(i12, i13, i12i13) <- xorGate
	(p0', i30, p0i30) <- orGate
	(p0p1in, p0'', p0p1p0) <- andNotBGate
	(p0p1p0in, i12i13in, a0) <- andGate
	(p0p1p0in', i13', a1) <- nandGate
	(p0p1in', p0i30in, a2) <- andGate
	connectWire0 p0p1 `mapM_` [p0p1in, p0p1in']
	connectWire0 p0p1p0 `mapM_` [p0p1p0in, p0p1p0in']
	connectWire0 i12i13 i12i13in
	connectWire0 p0i30 p0i30in

	connectWire (cout, 1, 6) `mapM_` [(p0, 1, 0), (p0', 1, 0), (p0'', 1, 0)]
	connectWire (cout, 1, 7) (p1, 1, 0)
	connectWire (iout, 1, 12) (i12, 1, 0)
	connectWire (iout, 1, 13) `mapM_` [(i13, 1, 0), (i13', 1, 0)]
	connectWire (iout, 1, 30) (i30, 1, 0)

	cz <- constGate 0
	connectWire (a0, 1, 0) (oin, 1, 0)
	connectWire (a1, 1, 0) (oin, 1, 1)
	connectWire (a2, 1, 0) (oin, 1, 2)
	connectWire (cz, 61, 0) (oin, 61, 3)

	return (cin, iin, oout)
