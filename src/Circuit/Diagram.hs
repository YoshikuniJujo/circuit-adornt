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

data CircuitDiagramElem
	= HLineD | TopLeftD | TopRightD | BottomLeftD | BottomRightD
	| NotGateD | AndGateD
	deriving Show

data CircuitDiagram = CircuitDiagram {
	cdTop :: Int8,
	cdBottom :: Int8,
	cdLeft :: Int8,
	cdDiagram :: Map (Int8, Int8) CircuitDiagramElem }
	deriving Show

sampleCircuitBuilder :: CircuitBuilder (IWire, IWire, IWire, OWire)
sampleCircuitBuilder = do
	(i0, o0) <- notGate
	(a1, a2, ao) <- andGate
	(a1', a2', ao') <- andGate
	(i1, o1) <- notGate
	(i2, o2) <- notGate
	connectWire64 o0 a1'
	connectWire64 ao a2'
	connectWire64 ao' i1
	connectWire64 o1 i2
	return (i0, a1, a2, o2)

initCircuitDiagram :: CircuitDiagram
initCircuitDiagram = CircuitDiagram {
	cdTop = 3,
	cdBottom = -3,
	cdLeft = 15,
	cdDiagram = empty }

toCircuitDiagram :: CircuitBuilder a -> OWire -> CircuitDiagram
toCircuitDiagram cb ow = fromCBState cbs ow (0, 0) initCircuitDiagram
	where cbs = cb `execState` initCBState

fromCBState :: CBState -> OWire -> (Int8, Int8) -> CircuitDiagram -> CircuitDiagram
fromCBState cbs ow pos@(x, y) cd = case cbsGate cbs !? ow of
	Just (NotGate iw) -> case getOWire cbs iw of
		Just ow' -> fromCBState cbs ow' (x + 3, y) cd''
		Nothing -> cd''
		where
		cd' = cd { cdDiagram = insert pos HLineD $ cdDiagram cd }
		cd'' = cd { cdDiagram = insert (x + 1, y) NotGateD $ cdDiagram cd' }
	Just (AndGate i1 i2) -> let
		cdcd1 = case getOWire cbs i1 of
			Just ow1 -> let
				cd3 = cd'' { cdDiagram = insert (x + 4, y + 1) TopRightD $ cdDiagram cd'' }
				cd4 = cd3 { cdDiagram = insert (x + 4, y + 2) BottomLeftD $ cdDiagram cd3 }
				cd5 = fromCBState cbs ow1 (x + 5, y + 2) cd4 in
				cd5
			Nothing -> cd''
		cdcd2 = case getOWire cbs i2 of
			Just ow1 -> let
				cd3 = cdcd1 { cdDiagram = insert (x + 4, y - 1) BottomRightD $ cdDiagram cdcd1 }
				cd4 = cd3 { cdDiagram = insert (x + 4, y - 2) TopLeftD $ cdDiagram cd3 }
				cd5 = fromCBState cbs ow1 (x + 5, y - 2) cd4 in
				cd5
			Nothing -> cd'' in
		cdcd2
		where
		cd' = cd { cdDiagram = insert pos HLineD $ cdDiagram cd }
		cd'' = cd' { cdDiagram = insert (x + 1, y) AndGateD $ cdDiagram cd' }
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
	Just HLineD -> moveTo ((- fromIntegral x) ^& fromIntegral y) hLineD
	Just TopLeftD -> moveTo ((- fromIntegral x) ^& fromIntegral y) topLeftD
	Just TopRightD -> moveTo ((- fromIntegral x) ^& fromIntegral y) topRightD
	Just BottomLeftD -> moveTo ((- fromIntegral x) ^& fromIntegral y) bottomLeftD
	Just BottomRightD -> moveTo ((- fromIntegral x) ^& fromIntegral y) bottomRightD
	Just NotGateD -> moveTo ((- fromIntegral x) ^& fromIntegral y) notGateD
	Just AndGateD -> moveTo ((- fromIntegral x) ^& fromIntegral y) andGateD
	_ -> mempty

sampleCircuitDiagram :: CircuitDiagram
sampleCircuitDiagram = toCircuitDiagram sampleCircuitBuilder ow
	where
	((_i1, _i2, _i3, ow), _) = sampleCircuitBuilder `runState` initCBState

tryDiagrams :: IO ()
tryDiagrams = renderSVG "sample.svg" (mkWidth 400) $ toDiagram sampleCircuitDiagram
