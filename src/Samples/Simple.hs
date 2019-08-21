{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}

module Samples.Simple () where

import Control.Arrow
import Control.Monad
import Data.Foldable
import Data.List
import Data.Bool
import Data.Word

import qualified Data.Bits as B

import Circuit.Adornt.Builder
import Circuit.Adornt.Parts

import Tools

sampleTri :: CircuitBuilder (IWire, IWire, IWire, IWire, OWire)
sampleTri = do
	(a1, b1, o1) <- triGate
	(a2, b2, o2) <- triGate
	(oin, oout) <- idGate
	connectWire64 o1 oin
	connectWire64 o2 oin
	return (a1, b1, a2, b2, oout)

decoder' :: Word8 -> CircuitBuilder (IWire, OWire)
decoder' n = do
	(oin, oout) <- idGate
	(iin, os) <- decoder $ fromIntegral n
	let	n = fromIntegral $ length os
	for_ (zip [0 ..] os) $ \(i, op) -> connectWire (op, 1, 0) (oin, 1, i)
	z <- constGate 0
	connectWire (z, 64 - n, 0) (oin, 64 - n, n)
	return (iin, oout)
