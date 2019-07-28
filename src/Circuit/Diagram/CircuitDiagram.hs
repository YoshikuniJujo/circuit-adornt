{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Circuit.Diagram.CircuitDiagram (
	CircuitDiagram(..), initCircuitDiagram, toDiagram,
	CircuitDiagramElem(..) ) where

import Prelude as P

import Data.Map.Strict
import Data.Int
import Diagrams.Prelude
import Diagrams.Backend.SVG

import Circuit.Diagram.Gates

data CircuitDiagram = CircuitDiagram {
	cdTop :: Int8,
	cdBottom :: Int8,
	cdLeft :: Int8,
	cdDiagram :: Map (Int8, Int8) CircuitDiagramElem }
	deriving Show

data CircuitDiagramElem
	= HLineD | VLineD | TopLeftD | TopRightD | BottomLeftD | BottomRightD
	| TLineD | LTLineD
	| NotGateD | AndGateD
	deriving Show

initCircuitDiagram :: CircuitDiagram
initCircuitDiagram = CircuitDiagram {
	cdTop = 8,
	cdBottom = -8,
	cdLeft = 23,
	cdDiagram = empty }

toDiagram :: CircuitDiagram -> Diagram B
toDiagram cd = toDiagramGen (cdTop cd) (cdBottom cd) (cdLeft cd) (cdDiagram cd)

toDiagramGen ::
	Int8 -> Int8 -> Int8 -> Map (Int8, Int8) CircuitDiagramElem -> Diagram B
toDiagramGen t b l ds = mconcat . (<$> [b .. t]) $ \y ->
	mconcat . (<$> [0 .. l]) $ \x -> toDiagram1 (x, y) ds

toDiagram1 :: (Int8, Int8) -> Map (Int8, Int8) CircuitDiagramElem -> Diagram B
toDiagram1 (x, y) ds = case ds !? (x, y) of
	Just HLineD -> moveTo ((- fromIntegral x) ^& fromIntegral y) hLineD
	Just VLineD -> moveTo ((- fromIntegral x) ^& fromIntegral y) vLineD
	Just TopLeftD -> moveTo ((- fromIntegral x) ^& fromIntegral y) topLeftD
	Just TopRightD -> moveTo ((- fromIntegral x) ^& fromIntegral y) topRightD
	Just BottomLeftD -> moveTo ((- fromIntegral x) ^& fromIntegral y) bottomLeftD
	Just BottomRightD -> moveTo ((- fromIntegral x) ^& fromIntegral y) bottomRightD
	Just TLineD -> moveTo ((- fromIntegral x) ^& fromIntegral y) tLineD
	Just LTLineD -> moveTo ((- fromIntegral x) ^& fromIntegral y) ltLineD
	Just NotGateD -> moveTo ((- fromIntegral x) ^& fromIntegral y) notGateD
	Just AndGateD -> moveTo ((- fromIntegral x) ^& fromIntegral y) andGateD
	_ -> mempty
