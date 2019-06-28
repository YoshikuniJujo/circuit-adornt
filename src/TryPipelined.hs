{-# OPTIONS_GHC -fno-warn-tabs #-}

module TryPipelined () where

import Circuit
import Clock
import Memory
import Control
import Pipelined
import SampleInstructions

((cl, pc, rim, ifId, rrf, mc, idEx), cct) = makeCircuit pipelined

cct1 = foldr (uncurry $ storeRiscvInstMem rim) cct
	$ zip [0, 4 ..] sampleInstControlInstructions

cct2 = resetPipelineRegisters ifId idEx cct1

cct3 = resetProgramCounter pc cct2

cct4 = clockOn cl cct3
