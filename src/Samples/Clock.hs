{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Samples.Clock where

import Data.Word

import Circuit
import Element

data Clock = Clock { clSwitch :: IWire, clSignal :: OWire } deriving Show

clock :: Word8 -> CircuitBuilder Clock
clock n = do
	(cin, cout) <- idGate
	(ni, no) <- notGate
	connectWire0 cout ni
	(sw, off, on, out) <- mux2
	connectWire0 cout off
	connectWire0 no on
	connectWire0 out cin
	delay cin n
	(oin, oout) <- idGate
	connectWire0 cout oin
	return Clock { clSwitch = sw, clSignal = oout }
