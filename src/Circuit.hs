{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Circuit (
	Circuit, makeCircuit, step, setBits, peekOWire, bitsToWord, wordToBits,
	CircuitBuilder, connectWire, connectWire64, connectWire0, delay,
		constGate, idGate, notGate, andGate, orGate, triGate,
	IWire, OWire, Bits(..), BitLen, BitPosIn, BitPosOut,
	setMultBits
	) where

import Data.Word

import CircuitCore

connectWire64 :: OWire -> IWire -> CircuitBuilder ()
connectWire64 o i = connectWire (o, 64, 0) (i, 64, 0)

connectWire0 :: OWire -> IWire -> CircuitBuilder ()
connectWire0 o i = connectWire (o, 1, 0) (i, 1, 0)

setMultBits :: [IWire] -> [Word64] -> Circuit -> Circuit
setMultBits is vs = foldr (.) id $ zipWith setBits is (wordToBits <$> vs)
