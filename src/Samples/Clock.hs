{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Samples.Clock where

import Data.Word

import Circuit
import Circuit.Adornt.Parts

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
	cz <- constGate $ Bits 0
	connectWire (cz, 63, 0) (oin, 63, 1)
	return Clock { clFrequency_2 = n,
		clInput = cin, clSwitch = sw, clSignal = oout }

clockSignal :: Clock -> OWire
clockSignal = clSignal

resetClock :: Clock -> Circuit -> Circuit
resetClock cl cct = (!! fromIntegral (2 * clFrequency_2 cl))
	. iterate (step . setBits (clInput cl) (Bits 0))
	$ setBits (clSwitch cl) (Bits 0) cct

clockOn :: Clock -> Circuit -> Circuit
clockOn cl cct = setBits (clSwitch cl) (Bits 1)
	. (!! fromIntegral (2 * clFrequency_2 cl))
	$ iterate (step . setBits (clInput cl) (Bits 0)) cct

squareWave :: IWire -> Bits -> Word8 -> Word8 -> Circuit -> Circuit
squareWave o b pre prd cct = let
	cct1 = (!! fromIntegral pre) . iterate step $ setBits o (Bits 0) cct
	cct2 = (!! fromIntegral prd) . iterate step $ setBits o b cct1
	cct3 = setBits o (Bits 0) cct2 in
	cct3
