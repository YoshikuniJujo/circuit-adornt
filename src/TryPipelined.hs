{-# OPTIONS_GHC -fno-warn-tabs #-}

module TryPipelined () where

import Circuit
import Clock
import Memory
import Pipelined

((cl, pc, ifId, pcsrc, pcbr), cct) = makeCircuit instructionFetch

cct1 = resetProgramCounter pc cct

cct2 = clockOn cl cct1
