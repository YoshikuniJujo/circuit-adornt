{-# OPTIONS_GHC -fno-warn-tabs #-}

module TryDataHazardsForward () where

import Circuit
import Clock
import Memory
import PipelinedForward
import MakeInstruction
import SampleInstructions

((cl, pc, rim, ifId, rrf, idEx, exMem, rdm, memWb, fa, fb), cct) = makeCircuit pipelined

cct0 = resetProgramCounter pc cct

cct1 = resetPipelineRegisters ifId idEx exMem memWb cct0

cct2 = foldr (uncurry $ storeRiscvInstMem rim) cct1
	. zip [0, 4 ..] $ encodeInst <$> sampleDataHazards

cct3 = foldr (uncurry $ storeRiscvRegisterFile rrf) cct2 $ zip
	[1, 2, 3, 5, 6, 15] [
		1234567918,	-- x1
		60,		-- x2
		1234567890,	-- x3
		0x00ff00ff002a,	-- x5
		0x7777777707,	-- x6
		5555555555 ]	-- x15

cct4 = resetProgramCounter pc cct3

cct5 = clockOn cl cct4

{-

sub x2, x1, x3
and x12, x2, x5
or x13, x6, x2
add x14, x2, x2
sd x15, 100(x2)

-}

cct300 = (!! 300) $ iterate step cct5
