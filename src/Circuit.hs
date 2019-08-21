{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Circuit (
	Circuit, makeCircuit, step, setBits, peekOWire, bitsToWord, wordToBits,
	CircuitBuilder, connectWire, connectWire64, connectWire0, connectWire0_64,
		delay, constGate, idGate, notGate, andGate, orGate, triGate, cheatGate,
	IWire, OWire, Bits(..), BitLen, BitPosIn, BitPosOut,
	setMultBits, peekMultOWires, Wire11, Wire21, Wire31, Wire41, Wire22
	) where

import Data.Word

import CircuitCore

setMultBits :: [IWire] -> [Word64] -> Circuit -> Circuit
setMultBits is vs = foldr (.) id $ zipWith setBits is (wordToBits <$> vs)

peekMultOWires :: [OWire] -> Circuit -> [Word64]
peekMultOWires os cct = bitsToWord . (`peekOWire` cct) <$> os
