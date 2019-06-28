{-# OPTIONS_GHC -fno-warn-tabs #-}

module TryPipelined () where

import Circuit
import Clock
import Memory
import Control
import Pipelined
import SampleInstructions

((cl, pc, rim, ifId, mc, rrf, idEx), cct) = makeCircuit pipelined

cct1 = foldr (uncurry $ storeRiscvInstMem rim) cct
	$ zip [0, 4 ..] sampleInstControlInstructions

cct2 = resetPipelineRegisters ifId idEx cct1

cct3 = resetProgramCounter pc cct2

cct4 = resetMainController mc cct3

cct5 = clockOn cl cct4

cct6 = stopProgramCounter pc rrf 100 cct5
