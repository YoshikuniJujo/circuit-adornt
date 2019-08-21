{-# LANGUAGE BinaryLiterals #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Samples.ControlPla where

import Circuit.Adornt.Builder
import Circuit.Adornt.Parts

controlPla :: CircuitBuilder (IWire, OWire)
controlPla = do
	(iin, iout) <- idGate
	(pin, pout) <- pla8 [
		(0b000, 0b00101011),
		(0b010, 0b00100100),
		(0b011, 0b10000010),
		(0b110, 0b01010000) ]
	connectWire (iout, 3, 4) (pin, 3, 0)
	cz <- constGate 0
	connectWire (cz, 5, 0) (pin, 5, 3)
	return (iin, pout)
