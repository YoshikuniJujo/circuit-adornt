{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Circuit (
	Circuit, makeCircuit, step, setBits, peekOWire, bitsToWord, wordToBits,
	CircuitBuilder, connectWire, connectWire64, delay,
		constGate, idGate, notGate, andGate, orGate, triGate,
	IWire, OWire, Bits(..), BitLen, BitPosIn, BitPosOut
	) where

import CircuitCore

connectWire64 :: OWire -> IWire -> CircuitBuilder ()
connectWire64 o i = connectWire (o, 64, 0) (i, 64, 0)
