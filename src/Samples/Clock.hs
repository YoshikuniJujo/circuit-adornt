{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Samples.Clock where

import Data.Word

import Circuit.Adornt.Simulator
import Circuit.Adornt.Parts
import Circuit.Adornt.Builder

data Clock = Clock {
	clFrequency_2 :: Word8,
	clSwitch :: IWire, clInput :: IWire, clSignal :: OWire }
	deriving Show

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
	cz <- constGate 0
	connectWire (cz, 63, 0) (oin, 63, 1)
	return Clock { clFrequency_2 = n,
		clInput = cin, clSwitch = sw, clSignal = oout }

clockSignal :: Clock -> OWire
clockSignal = clSignal

resetClock :: Clock -> CircuitSimulator -> CircuitSimulator
resetClock cl cct = (!! fromIntegral (2 * clFrequency_2 cl))
	. iterate (step . setBits (clInput cl) (wordToBits 0))
	$ setBits (clSwitch cl) (wordToBits 0) cct

clockOn :: Clock -> CircuitSimulator -> CircuitSimulator
clockOn cl cct = setBits (clSwitch cl) (wordToBits 1)
	. (!! fromIntegral (2 * clFrequency_2 cl))
	$ iterate (step . setBits (clInput cl) (wordToBits 0)) cct

squareWave :: IWire -> Bits -> Word8 -> Word8 -> CircuitSimulator -> CircuitSimulator
squareWave o b pre prd cct = let
	cct1 = (!! fromIntegral pre) . iterate step $ setBits o (wordToBits 0) cct
	cct2 = (!! fromIntegral prd) . iterate step $ setBits o b cct1
	cct3 = setBits o (wordToBits 0) cct2 in
	cct3
