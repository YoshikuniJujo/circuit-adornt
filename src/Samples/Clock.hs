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

clockSignal :: Clock -> OWire
clockSignal = clSignal

clockOn :: Clock -> Circuit -> Circuit
clockOn cl = setBits (clSwitch cl) (Bits 1)

squareWave :: IWire -> Bits -> Word8 -> Word8 -> Circuit -> Circuit
squareWave o b pre prd cct = let
	cct1 = (!! fromIntegral pre) . iterate step $ setBits o (Bits 0) cct
	cct2 = (!! fromIntegral prd) . iterate step $ setBits o b cct1
	cct3 = setBits o (Bits 0) cct2 in
	cct3
