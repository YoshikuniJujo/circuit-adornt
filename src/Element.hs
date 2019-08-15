{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Element (
	xorGate, nandGate, andNotBGate, andGate3, andGate4, orGate4, xorGate3,
	mux2, mux3, mux4, multiplexer, decoder, pla8, testZero,
	dlatch, dflipflop
	) where

import Data.Foldable

import Circuit
import Circuit.Adornt.Parts

testZero :: CircuitBuilder (IWire, OWire)
testZero = do
	(iin, iout) <- idGate
	(ois, oo) <- multiple orGate 64
	(ni, no) <- notGate
	for_ (zip [0 ..] ois) $ \(i, oi) -> connectWire (iout, 1, i) (oi, 1, 0)
	connectWire0 oo ni
	(oin, oout) <- idGate
	connectWire0 no oin
	cz <- constGate $ Bits 0
	connectWire (cz, 63, 0) (oin, 63, 1)
	return (iin, oout)
