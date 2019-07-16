-- {-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Circuit.Diagram.AndGate (andGateD) where

import Diagrams.Prelude
import Diagrams.Direction
import Diagrams.Backend.SVG

andGateD :: Diagram B
andGateD = moveTo ((- 1.5) ^& 0) $ (inputs <> andGatePure <> moveTo (1.5 ^& 0) (lineRight (- 0.2)))
	`withEnvelope'` (rect 3 3 :: Diagram B)

inputs, input1, input2 :: Diagram B
inputs = input1 <> input2
input1 = moveTo ((- 1.5) ^& 1) $ lineRight 0.2
input2 = moveTo ((- 1.5) ^& (- 1)) $ lineRight 0.2

lineRight :: Double -> Diagram B
lineRight l = strokeT (fromOffsets [zero &_x .~ l]) # lwL 0.08

andGatePure, andGate1, andGate2 :: Diagram B
andGatePure = andGate1 <> andGate2
andGate1 = fromVertices (map p2 [(0, 1.3), (-1.3, 1.3), (-1.3, -1.3), (0, -1.3)]) # lwL 0.08
andGate2 = moveTo (0 ^& 0) $ scale 1.3 (arc (dir unit_Y) (1 / 2 @@ turn)) # lwL 0.08

withEnvelope' :: (InSpace v n a, Monoid' m, Enveloped a) =>
	QDiagram b v n m -> a -> QDiagram b v n m
withEnvelope' = flip withEnvelope
