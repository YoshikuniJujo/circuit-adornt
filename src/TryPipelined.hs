{-# OPTIONS_GHC -fno-warn-tabs #-}

module TryPipelined () where

import Circuit
import Clock
import Memory
import Pipelined
import SampleInstructions

((cl, pc, rim, ifId, pcsrc, pcbr), cct) = makeCircuit instructionFetch

cct1 = foldr (uncurry $ storeRiscvInstMem rim) cct
	$ zip [0, 4 ..] sampleInstControlInstructions

cct2 = resetProgramCounter pc cct1

cct3 = clockOn cl cct2
