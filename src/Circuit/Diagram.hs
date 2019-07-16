{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Circuit.Diagram where

import Control.Monad.State
import Data.Map.Strict
import Data.Int
import Diagrams.Prelude
import Diagrams.Backend.SVG

import Circuit
import CircuitCore
import CircuitTypes

import Circuit.Diagram.Gates

data CircuitDiagramElem = NotGateD
	deriving Show

data CircuitDiagram = CircuitDiagram {
	cdTop :: Int8,
	cdBottom :: Int8,
	cdLeft :: Int8,
	cdDiagram :: Map (Int8, Int8) CircuitDiagramElem }
	deriving Show

sampleCircuitBuilder :: CircuitBuilder (IWire, OWire)
sampleCircuitBuilder = do
	(i0, o0) <- notGate
	(i1, o1) <- notGate
	(i2, o2) <- notGate
	(i3, o3) <- notGate
	connectWire64 o0 i1
	connectWire64 o1 i2
	connectWire64 o2 i3
	return (i0, o3)

initCircuitDiagram :: CircuitDiagram
initCircuitDiagram = CircuitDiagram {
	cdTop = 1,
	cdBottom = -1,
	cdLeft = 10,
	cdDiagram = empty }

toCircuitDiagram :: CircuitBuilder a -> OWire -> CircuitDiagram
toCircuitDiagram cb ow = fromCBState cbs ow (0, 0) initCircuitDiagram
	where cbs = cb `execState` initCBState

fromCBState :: CBState -> OWire -> (Int8, Int8) -> CircuitDiagram -> CircuitDiagram
fromCBState cbs ow pos@(x, y) cd = case cbsGate cbs !? ow of
	Just (NotGate iw) -> case getOWire cbs iw of
		Just ow' -> fromCBState cbs ow' (x + 2, y) cd'
		Nothing -> cd'
		where cd' = cd { cdDiagram = insert pos NotGateD $ cdDiagram cd }
	_ -> error "yet"

getOWire :: CBState -> IWire -> Maybe OWire
getOWire cbs iw = fst . head <$> cbsWireConn cbs !? iw

toDiagram :: CircuitDiagram -> Diagram B
toDiagram cd = toDiagramGen (cdTop cd) (cdBottom cd) (cdLeft cd) (cdDiagram cd)

toDiagramGen ::
	Int8 -> Int8 -> Int8 -> Map (Int8, Int8) CircuitDiagramElem -> Diagram B
toDiagramGen t b l ds = mconcat . (<$> [b .. t]) $ \y ->
	mconcat . (<$> [0 .. l]) $ \x -> toDiagram1 (x, y) ds

toDiagram1 :: (Int8, Int8) -> Map (Int8, Int8) CircuitDiagramElem -> Diagram B
toDiagram1 (x, y) ds = case ds !? (x, y) of
	Just NotGateD -> moveTo ((- fromIntegral x) ^& fromIntegral y) notGateD
	_ -> mempty

sampleCircuitDiagram :: CircuitDiagram
sampleCircuitDiagram = toCircuitDiagram sampleCircuitBuilder ow
	where
	((_iw, ow), _) = sampleCircuitBuilder `runState` initCBState

tryDiagrams :: IO ()
tryDiagrams = renderSVG "sample.svg" (mkWidth 400) $ toDiagram sampleCircuitDiagram
