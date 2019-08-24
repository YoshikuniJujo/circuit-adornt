{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Circuit.Adornt (

-- * Simulator

-- ** Circuit Simulator
CircuitSimulator, prepareSimulator, prepareSimulatorRandom, prepareSimulatorRandomIO, step,

-- ** Set Bits of Input Wire
IWire, setBits, setMultiBits,

-- ** Peek Bits of Output wire
OWire, peekOWire, peekMultiOWires,

-- ** Convert between Bits and Word64
Bits, bitsToWord, wordToBits,

-- * Diagram
renderSVG, mkWidth, mkHeight, dims2D,
circuitDiagram,

-- * Builder

-- ** Circuit Builder
CircuitBuilder,

-- ** Basic Gates
constGate, idGate, andGate, orGate, notGate, triGate, cheatGate,

-- ** Delay
delay,

-- ** Wire Sets
Wire11, Wire21, Wire31, Wire41, Wire51, Wire22, Wire32,

-- ** Wire Connection
connectWire, connectWire0, connectWire64, connectWire0_64,
BitLen, BitPosIn, BitPosOut,

-- ** Named Block
putNamedBlock, BlockName,

) where

import Diagrams.Prelude

import Circuit.Adornt.Simulator
import Circuit.Adornt.Builder
import Circuit.DiagramDsl

import qualified Circuit.Adornt.Diagram as D
import qualified Diagrams.Backend.SVG as S

circuitDiagram :: Int -> CircuitBuilder [OWire] -> Either String (Diagram S.B)
circuitDiagram n = either Left (Right . drawDiagram) . D.circuitDiagram n

renderSVG :: FilePath -> SizeSpec V2 Double -> Diagram S.B -> IO ()
renderSVG = S.renderSVG
