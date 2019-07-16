{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Circuit.Diagram where

import Prelude as P

import Control.Monad.State
import Data.Map.Strict
import Data.Bool
import Data.Int
import Diagrams.Prelude
import Diagrams.Backend.SVG

import Circuit
import CircuitCore
import CircuitTypes

import Circuit.Diagram.Gates

type DiagramBuilder = State (Map BasicGate [(Bool, (Int8, Int8))])

fromCBStateBuilder :: Word -> CBState -> OWire -> (Int8, Int8) -> CircuitDiagram -> DiagramBuilder CircuitDiagram
fromCBStateBuilder n cbs ow pos cd = do
	let	bgt = cbsGate cbs ! ow
	mold <- gets (!? bgt)
	case mold of
		Just ps -> do
			modify $ insert bgt ((False, pos) : ps)
			return $ fromTo pos (head ps) cd
		Nothing -> do
			modify $ insert bgt [(True, pos)]
			newFromCBStateBuilder n cbs bgt pos cd

fromTo :: (Int8, Int8) -> (Bool, (Int8, Int8)) -> CircuitDiagram -> CircuitDiagram
fromTo (x, y) (org, (x', y')) cd
	| x == x' && y' > y = let
		cd' = cd { cdDiagram = insert (x, y) TopRightD $ cdDiagram cd }
		cd'' = cd { cdDiagram = P.foldr (\yy -> insert (x, yy) VLineD) (cdDiagram cd') [y + 1 .. y' - 1] }
		cd''' = cd'' { cdDiagram = insert (x, y') tl (cdDiagram cd'') } in
		cd'''
	| x > x' && y' > y = let
		cd' = cd { cdDiagram = insert (x, y) TopRightD (cdDiagram cd) }
		cd'' = cd' { cdDiagram = insert (x, y + 1) BottomRightD (cdDiagram cd') }
		cd''' = cd'' { cdDiagram = P.foldr (\xx -> insert (xx, y + 1) HLineD) (cdDiagram cd'') [x'+ 1 .. x - 1] }
		cd4 = cd''' { cdDiagram = insert (x', y + 1) TopLeftD (cdDiagram cd''') }
		cd5 = cd4 { cdDiagram = P.foldr (\yy -> insert (x', yy) VLineD) (cdDiagram cd4) [y + 2 .. y' - 1] } in
		cd5 { cdDiagram = insert (x', y') tl (cdDiagram cd5) }
	where
	tl = bool LTLineD TLineD org

newFromCBStateBuilder :: Word -> CBState -> BasicGate -> (Int8, Int8) -> CircuitDiagram -> DiagramBuilder CircuitDiagram
newFromCBStateBuilder n cbs bgt pos@(x, y) cd =
	case bgt of
		NotGate iw -> case getOWire cbs iw of
			Just ow' -> fromCBStateBuilder n cbs ow' (x + 3, y) cd''
			Nothing -> return cd''
			where
			cd' = cd { cdDiagram = insert pos HLineD $ cdDiagram cd }
			cd'' = cd { cdDiagram = insert (x + 1, y) NotGateD $ cdDiagram cd' }
		AndGate i1 i2 -> do
			cdcd1 <- case getOWire cbs i1 of
				Just ow1 -> let
					cd3 = cd'' { cdDiagram = insert (x + 4, y + 1) TopRightD $ cdDiagram cd'' }
					cd4 = cd3 { cdDiagram = P.foldr (\dy -> insert (x + 4, y + dy) VLineD) (cdDiagram cd3) [2 .. fromIntegral n - 1] }
					cd5 = cd4 { cdDiagram = insert (x + 4, y + fromIntegral n) BottomLeftD $ cdDiagram cd4 } in
					fromCBStateBuilder (n `div` 2) cbs ow1 (x + 5, y + fromIntegral n) cd5
				Nothing -> return cd''
			cdcd2 <- case getOWire cbs i2 of
				Just ow1 -> let
					cd3 = cdcd1 { cdDiagram = insert (x + 4, y - 1) BottomRightD $ cdDiagram cdcd1 }
					cd4 = cd3 { cdDiagram = P.foldr (\dy -> insert (x + 4, y - dy) VLineD) (cdDiagram cd3) [2 .. fromIntegral n - 1] }
					cd5 = cd4 { cdDiagram = insert (x + 4, y - fromIntegral n) TopLeftD $ cdDiagram cd4 } in
					fromCBStateBuilder (n `div` 2) cbs ow1 (x + 5, y - fromIntegral n) cd5
				Nothing -> return cdcd1
			return cdcd2
			where
			cd' = cd { cdDiagram = insert pos HLineD $ cdDiagram cd }
			cd'' = cd' { cdDiagram = insert (x + 1, y) AndGateD $ cdDiagram cd' }
		_ -> error "yet"

data CircuitDiagramElem
	= HLineD | VLineD | TopLeftD | TopRightD | BottomLeftD | BottomRightD
	| TLineD | LTLineD
	| NotGateD | AndGateD
	deriving Show

data CircuitDiagram = CircuitDiagram {
	cdTop :: Int8,
	cdBottom :: Int8,
	cdLeft :: Int8,
	cdDiagram :: Map (Int8, Int8) CircuitDiagramElem }
	deriving Show

sampleCircuitBuilder :: CircuitBuilder (IWire, IWire, IWire, IWire, IWire, OWire)
sampleCircuitBuilder = do
	(a_21, a_22, a_2o) <- andGate
	(a_11, a_12, a_1o) <- andGate
	(i_1, o_1) <- notGate
	(i0, o0) <- notGate
	(a1, a2, ao) <- andGate
	(a1', a2', ao') <- andGate
	(i1, o1) <- notGate
	(i2, o2) <- notGate
	connectWire64 o_1 i0
	connectWire64 o0 a1'
	connectWire64 a_2o a_11
	connectWire64 a_1o a1
	connectWire64 ao a2'
	connectWire64 ao' i1
	connectWire64 o1 i2
	return (i_1, a_21, a_22, a_12, a2, o2)

sampleCircuitBuilder2 :: CircuitBuilder (IWire, OWire)
sampleCircuitBuilder2 = do
	(ni, no) <- notGate
	(a1, a2, ao) <- andGate
	(a1', a2', ao') <- andGate
	(a1'', a2'', ao'') <- andGate
	(ni', no') <- notGate
	connectWire64 no a1
	connectWire64 no a2
	connectWire64 no a1'
	connectWire64 no a2'
	connectWire64 ao a1''
	connectWire64 ao' ni'
	connectWire64 no' a2''
	return (ni, ao'')

initCircuitDiagram :: CircuitDiagram
initCircuitDiagram = CircuitDiagram {
	cdTop = 8,
	cdBottom = -8,
	cdLeft = 23,
	cdDiagram = empty }

toCircuitDiagram :: Word -> CircuitBuilder a -> OWire -> CircuitDiagram
toCircuitDiagram n cb ow = fromCBStateBuilder n cbs ow (0, 0) initCircuitDiagram `evalState` empty
	where cbs = cb `execState` initCBState

fromCBState :: Word -> CBState -> OWire -> (Int8, Int8) -> CircuitDiagram -> CircuitDiagram
fromCBState n cbs ow pos@(x, y) cd = case cbsGate cbs !? ow of
	Just (NotGate iw) -> case getOWire cbs iw of
		Just ow' -> fromCBState n cbs ow' (x + 3, y) cd''
		Nothing -> cd''
		where
		cd' = cd { cdDiagram = insert pos HLineD $ cdDiagram cd }
		cd'' = cd { cdDiagram = insert (x + 1, y) NotGateD $ cdDiagram cd' }
	Just (AndGate i1 i2) -> let
		cdcd1 = case getOWire cbs i1 of
			Just ow1 -> let
				cd3 = cd'' { cdDiagram = insert (x + 4, y + 1) TopRightD $ cdDiagram cd'' }
				cd4 = cd3 { cdDiagram = P.foldr (\dy -> insert (x + 4, y + dy) VLineD) (cdDiagram cd3) [2 .. fromIntegral n - 1] }
				cd5 = cd4 { cdDiagram = insert (x + 4, y + fromIntegral n) BottomLeftD $ cdDiagram cd4 }
				cd6 = fromCBState (n `div` 2) cbs ow1 (x + 5, y + fromIntegral n) cd5 in
				cd6
			Nothing -> cd''
		cdcd2 = case getOWire cbs i2 of
			Just ow1 -> let
				cd3 = cdcd1 { cdDiagram = insert (x + 4, y - 1) BottomRightD $ cdDiagram cdcd1 }
				cd4 = cd3 { cdDiagram = P.foldr (\dy -> insert (x + 4, y - dy) VLineD) (cdDiagram cd3) [2 .. fromIntegral n - 1] }
				cd5 = cd4 { cdDiagram = insert (x + 4, y - fromIntegral n) TopLeftD $ cdDiagram cd4 }
				cd6 = fromCBState (n `div` 2) cbs ow1 (x + 5, y - fromIntegral n) cd5 in
				cd6
			Nothing -> cdcd1 in
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

sampleCircuitDiagram, sampleCircuitDiagram2 :: CircuitDiagram
sampleCircuitDiagram = toCircuitDiagram 8 sampleCircuitBuilder ow
	where ((_, _i0, _i1, _i2, _i3, ow), _) = sampleCircuitBuilder `runState` initCBState
sampleCircuitDiagram2 = toCircuitDiagram 4 sampleCircuitBuilder2 ow
	where ((_, ow), _) = sampleCircuitBuilder2 `runState` initCBState

tryDiagrams, tryDiagrams2 :: IO ()
tryDiagrams = renderSVG "sample.svg" (mkWidth 400) $ toDiagram sampleCircuitDiagram
tryDiagrams2 = renderSVG "sample2.svg" (mkWidth 400) $ toDiagram sampleCircuitDiagram2
