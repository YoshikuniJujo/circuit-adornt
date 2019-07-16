{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Circuit.Diagram.Gates (
	andGateD, notGateD,
	hLineD, vLineD, topLeftD, topRightD, bottomLeftD, bottomRightD,
	tLineD, ltLineD
	) where

import Diagrams.Prelude
import Diagrams.Backend.SVG

import Circuit.Diagram.AndGate

notGateD :: Diagram B
notGateD = moveTo ((- 1) ^& 0)
	$ (moveTo ((- 1) ^& 0) (lineRight 0.1) <> notGateDPure <> moveTo (1 ^& 0) (lineRight (- 0.15)))
		`withEnvelope'` (rect 2 3 :: Diagram B)

notGateDPure :: Diagram B
notGateDPure = (moveTo ((- 0.45) ^& 0) (triangle1_4 1.5)  <>  moveTo (0.66 ^& 0) (circleB (1.5 / 8))) # lwL 0.08

lineRight :: Double -> Diagram B
lineRight l = strokeT (fromOffsets [zero &_x .~ l]) # lwL 0.08

lineUp :: Double -> Diagram B
lineUp l = strokeT (fromOffsets [zero &_y .~ l]) # lwL 0.08

withEnvelope' :: (InSpace v n a, Monoid' m, Enveloped a) =>
	QDiagram b v n m -> a -> QDiagram b v n m
withEnvelope' = flip withEnvelope

triangle1_4 :: Double -> Diagram B
triangle1_4 = rotateBy (- 1 / 4) . triangle

circleB :: Double -> Diagram B
circleB = circle

hLineD, vLineD :: Diagram B
hLineD = moveTo ((- 1) ^& 0)
	$ (strokeT (fromOffsets [unitX]) # lwL 0.08) `withEnvelope'` (rect 1 1 :: Diagram B)

vLineD = moveTo ((- 0.5) ^& (- 0.5)) $ strokeT (fromOffsets [unitY]) # lwL 0.08

topLeftD, topRightD, bottomLeftD, bottomRightD :: Diagram B
topLeftD = reflectY bottomLeftD
topRightD = (strokeT (fromOffsets [zero &_x .~ (- 0.5), zero &_y .~ 0.5]) # lwL 0.08) `withEnvelope'` (rect 2 1 :: Diagram B)
bottomLeftD = moveTo ((- 1) ^& 0) $ rotateBy (1 / 2) topRightD
bottomRightD = reflectY topRightD

tLineD, ltLineD :: Diagram B
tLineD = lineRight (- 1) <> moveTo ((- 0.5) ^& 0) (circle $ 1 / 8) # fc black <> moveTo ((- 0.5) ^& 0) (lineUp $ - 0.5)
ltLineD = moveTo ((- 0.5) ^& 0.5) $ rotateBy (1 / 4) tLineD
