{-# OPTIONS_GHC -fno-warn-tabs #-}

module TryLoadHazard () where

import Circuit
import Clock
import Memory
import PipelinedForward
import MakeInstruction
import SampleInstructions

{-

ld x2, 20(x1)
and x4, x2, x5
or x8, x2, x6
add x9, x4, x2
sub x1, x6, x7

-}

((cl, pc, rim, ifId, rrf, idEx, exMem, rdm, memWb), cct) = makeCircuit pipelined

cct0 = resetProgramCounter pc cct

cct1 = resetPipelineRegisters ifId idEx exMem memWb cct0

cct2 = foldr (uncurry $ storeRiscvInstMem rim) cct1
	. zip [0, 4 ..] $ encodeInst <$> sampleLoadHazards

cct3 = foldr (uncurry $ storeRiscvRegisterFile rrf) cct2 $ zip
	[1, 2, 5, 6, 7] [
		36,		-- x1
		0xff00ff00,	-- x2
		0x12345678,	-- x5
		0x87654321,	-- x6
		0x76543210 ]	-- x7

cct4 = foldr (uncurry $ storeRiscvDataMem rdm) cct3
	$ zip [0, 56] [5555555555, 0x00ff00ff]

cct5 = resetProgramCounter pc cct4

cct6 = clockOn cl cct5

cct300 = (!! 300) $ iterate step cct6
