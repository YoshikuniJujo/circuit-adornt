{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Samples.ImmGen where

import Circuit.Adornt.Builder
import Circuit.Adornt.Parts

immGen :: CircuitBuilder (IWire, OWire)
immGen = do
	(iin, iout) <- idGate
	(oin, oout) <- idGate
	connectWire (iout, 1, 31) (oin, 52, 12)
	connectWire (iout, 6, 25) (oin, 6, 5)
	(s1, is, sb, ii) <- mux2
	connectWire (iout, 1, 6) (s1, 1, 0)
	connectWire (iout, 1, 31) (is, 1, 11)
	connectWire (iout, 1, 7) (sb, 1, 11)
	connectWire (ii, 1, 11) (oin, 1, 11)
	(s2, i, ssb, fo) <- mux2
	connectWire (iout, 1, 5) (s2, 1, 0)
	connectWire (iout, 4, 21) (i, 4, 1)
	connectWire (iout, 4, 8) (ssb, 4, 1)
	connectWire (fo, 4, 1) (oin, 4, 1)
	(s3, i', s', sb', z) <- mux3
	cz <- constGate 0
	connectWire (iout, 2, 5) (s3, 2, 0)
	connectWire (iout, 1, 20) (i', 1, 0)
	connectWire (iout, 1, 7) (s', 1, 0)
	connectWire (cz, 1, 0) (sb', 1, 0)
	connectWire (z, 1, 0) (oin, 1, 0)
	return (iin, oout)
