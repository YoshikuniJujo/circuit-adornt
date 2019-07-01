{-# LANGUAGE BinaryLiterals #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module ControlPla where

import Circuit
import Element

controlPla :: CircuitBuilder (IWire, OWire)
controlPla = do
	(iin, iout) <- idGate64
	(pin, pout) <- pla8 [
		(0b000, 0b11110000),
		(0b010, 0b10001000),
		(0b011, 0b00100010),
		(0b110, 0b00000101) ]
	connectWire (iout, 3, 4) (pin, 3, 0)
	return (iin, pout)

controlPla' :: CircuitBuilder (IWire, OWire)
controlPla' = do
	(iin, iout) <- idGate64
	(pin, pout) <- pla8 [
		(0b000, 0b00101011),
		(0b010, 0b00100100),
		(0b011, 0b10000010),
		(0b110, 0b01010000) ]
	connectWire (iout, 3, 4) (pin, 3, 0)
	return (iin, pout)
