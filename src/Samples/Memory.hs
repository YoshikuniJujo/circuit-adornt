{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Samples.Memory where

import Circuit
import Element

srlatch :: CircuitBuilder Wire22
srlatch = do
	(r, nqin, qout) <- norGate
	(s, qin, nqout) <- norGate
	connectWire64 nqout nqin
	connectWire64 qout qin
	return (r, s, qout, nqout)
