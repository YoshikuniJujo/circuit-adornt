{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Circuit (
	Circuit, makeCircuit, step, setBits, peekOWire, bitsToWord, wordToBits,
	CircuitBuilder, connectWire, connectWire64, connectWire0, connectWire0_64,
		delay, constGate, idGate, notGate, andGate, orGate, triGate,
	IWire, OWire, Bits(..), BitLen, BitPosIn, BitPosOut,
	setMultBits, peekMultOWires, Wire11, Wire21, Wire31, Wire41, Wire22
	) where

import Data.Word

import CircuitCore

type Wire11 = (IWire, OWire)
type Wire21 = (IWire, IWire, OWire)
type Wire31 = (IWire, IWire, IWire, OWire)
type Wire41 = (IWire, IWire, IWire, IWire, OWire)

type Wire22 = (IWire, IWire, OWire, OWire)

connectWire64 :: OWire -> IWire -> CircuitBuilder ()
connectWire64 o i = connectWire (o, 64, 0) (i, 64, 0)

connectWire0 :: OWire -> IWire -> CircuitBuilder ()
connectWire0 o i = connectWire (o, 1, 0) (i, 1, 0)

connectWire0_64 :: OWire -> IWire -> CircuitBuilder ()
connectWire0_64 o i = connectWire (o, 1, 0) (i, 64, 0)

setMultBits :: [IWire] -> [Word64] -> Circuit -> Circuit
setMultBits is vs = foldr (.) id $ zipWith setBits is (wordToBits <$> vs)

peekMultOWires :: [OWire] -> Circuit -> [Word64]
peekMultOWires os cct = bitsToWord . (`peekOWire` cct) <$> os
