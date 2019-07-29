{-# LANGUAGE TupleSections, TypeFamilies #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Circuit.Diagram where

import Prelude as P

import Control.Monad.State
import Data.Maybe
import Data.Map.Strict
import Data.Int
import Diagrams.Prelude
import Diagrams.Backend.SVG

import Circuit
import CircuitCore
import CircuitTypes

import Circuit.Diagram.CircuitDiagram
import Circuit.Diagram.Route

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

data Dir = T | B | L | R deriving Show

routeToLines ::
	Bool -> Dir -> [(Int8, Int8)] -> [((Int8, Int8), CircuitDiagramElem)]
routeToLines False _ [(x, y)] = [((x, y), LTLineD)]
routeToLines True _ [(x, y)] = [((x, y), TLineD)]
routeToLines org L ((x, y) : xys@((x', y') : _))
	| x == x' && y + 1 == y' = ((x, y), TopRightD) : routeToLines org T xys
	| x == x' && y - 1 == y' = ((x, y), BottomRightD) : routeToLines org B xys
	| x + 1 == x' && y == y' = ((x, y), HLineD) : routeToLines org L xys
	| x - 1 == x' && y == y' = ((x, y), HLineD) : routeToLines org R xys
routeToLines org T ((x, y) : xys@((x', y') : _))
	| x == x' && y + 1 == y' = ((x, y), VLineD) : routeToLines org T xys
	| x - 1 == x' && y == y' = ((x, y), BottomRightD) : routeToLines org R xys
	| x + 1 == x' && y == y' = ((x, y), BottomLeftD) : routeToLines org L xys
routeToLines org R ((x, y) : xys@((x', y') : _))
	| x - 1 == x' && y == y' = ((x, y), HLineD) : routeToLines org R xys
	| x == x' && y + 1 == y' = ((x, y), TopLeftD) : routeToLines org T xys
	| x == x' && y - 1 == y' = ((x, y), BottomLeftD) : routeToLines org B xys
routeToLines org B ((x, y) : xys@((x', y') : _))
	| x == x' && y - 1 == y' = ((x, y), VLineD) : routeToLines org B xys
	| x - 1 == x' && y == y' = ((x, y), TopRightD) : routeToLines org R xys
routeToLines org dir xys = error $ "routeToLines: " ++ show org ++ " " ++ show dir ++ " " ++ show xys

routeToLines' :: Dir -> [(Int8, Int8)] -> [((Int8, Int8), CircuitDiagramElem)]
routeToLines' _ [(x, y)] = [((x, y), HLineD)]
routeToLines' L ((x, y) : xys@((x', y') : _))
	| x == x' && y + 1 == y' = ((x, y), TopRightD) : routeToLines' T xys
	| x == x' && y - 1 == y' = ((x, y), BottomRightD) : routeToLines' B xys
	| x + 1 == x' && y == y' = ((x, y), HLineD) : routeToLines' L xys
	| x - 1 == x' && y == y' = ((x, y), HLineD) : routeToLines' R xys
routeToLines' T ((x, y) : xys@((x', y') : _))
	| x == x' && y + 1 == y' = ((x, y), VLineD) : routeToLines' T xys
	| x - 1 == x' && y == y' = ((x, y), BottomRightD) : routeToLines' R xys
	| x + 1 == x' && y == y' = ((x, y), BottomLeftD) : routeToLines' L xys
routeToLines' R ((x, y) : xys@((x', y') : _))
	| x - 1 == x' && y == y' = ((x, y), HLineD) : routeToLines' R xys
	| x == x' && y + 1 == y' = ((x, y), TopLeftD) : routeToLines' T xys
	| x == x' && y - 1 == y' = ((x, y), BottomLeftD) : routeToLines' B xys
routeToLines' B ((x, y) : xys@((x', y') : _))
	| x == x' && y - 1 == y' = ((x, y), VLineD) : routeToLines' B xys
	| x - 1 == x' && y == y' = ((x, y), TopRightD) : routeToLines' R xys
routeToLines' dir xys = error $ "routeToLines': " ++ " " ++ show dir ++ " " ++ show xys

fromTo :: (Int8, Int8) -> (Bool, (Int8, Int8)) -> CircuitDiagram -> CircuitDiagram
fromTo (x, y) (org, (x', y')) cd = cd {
	cdDiagram = P.foldr (uncurry insert) (cdDiagram cd)
		. routeToLines org L . fromJust $ route (x, y) (x', y') cd }

fromTo' :: (Int8, Int8) -> (Int8, Int8) -> CircuitDiagram -> CircuitDiagram
fromTo' (x, y) (x', y') cd = cd {
	cdDiagram = P.foldr (uncurry insert) (cdDiagram cd)
		. routeToLines' L . fromJust $ route (x, y) (x', y') cd }

newFromCBStateBuilder :: Word -> CBState -> BasicGate -> (Int8, Int8) -> CircuitDiagram -> DiagramBuilder CircuitDiagram
newFromCBStateBuilder n cbs bgt pos@(x, y) cd =
	case bgt of
		NotGate iw -> case getOWire cbs iw of
			Just ow' -> fromCBStateBuilder n cbs ow' (x + 5, y) $ fromTo' (x, y) (x + 2, y) cd''
			Nothing -> return cd''
			where
			cd' = cd { cdDiagram = insert pos HLineD $ cdDiagram cd }
			cd'' = cd { cdDiagram = stump (x + 3, y) 2 3 . insert (x + 3, y) NotGateD $ cdDiagram cd' }
		AndGate i1 i2 -> do
			cdcd1 <- case getOWire cbs i1 of
				Just ow1 -> fromCBStateBuilder (n `div` 2) cbs ow1 (x + 6, y + 1) $ fromTo' (x, y) (x + 2, y) cd''
				Nothing -> return cd''
			cdcd2 <- case getOWire cbs i2 of
				Just ow1 -> fromCBStateBuilder (n `div` 2) cbs ow1 (x + 6, y - 1) cdcd1
				Nothing -> return cdcd1
			return cdcd2
			where
			cd' = cd { cdDiagram = insert pos HLineD $ cdDiagram cd }
			cd'' = cd' { cdDiagram = stump (x + 3, y) 3 3 . insert (x + 3, y) AndGateD $ cdDiagram cd' }
		_ -> error "yet"

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

toCircuitDiagram :: Word -> CircuitBuilder a -> OWire -> CircuitDiagram
toCircuitDiagram n cb ow = fromCBStateBuilder n cbs ow (0, 0) initCircuitDiagram `evalState` empty
	where cbs = cb `execState` initCBState

getOWire :: CBState -> IWire -> Maybe OWire
getOWire cbs iw = fst . head <$> cbsWireConn cbs !? iw

sampleCircuitDiagram, sampleCircuitDiagram2 :: CircuitDiagram
sampleCircuitDiagram = toCircuitDiagram 8 sampleCircuitBuilder ow
	where ((_, _i0, _i1, _i2, _i3, ow), _) = sampleCircuitBuilder `runState` initCBState
sampleCircuitDiagram2 = toCircuitDiagram 4 sampleCircuitBuilder2 ow
	where ((_, ow), _) = sampleCircuitBuilder2 `runState` initCBState

tryDiagrams, tryDiagrams2 :: IO ()
tryDiagrams = renderSVG "sample.svg" (mkWidth 400) $ toDiagram sampleCircuitDiagram
tryDiagrams2 = renderSVG "sample2.svg" (mkWidth 400) $ toDiagram sampleCircuitDiagram2

stump :: (Int8, Int8) -> Int8 -> Int8 -> Map (Int8, Int8) CircuitDiagramElem ->
	Map (Int8, Int8) CircuitDiagramElem
stump (x0, y0) w h m = P.foldr (\p -> insert p Stump) m [ (x, y) |
	x <- [x0 .. x0 + w'], y <- [y0 - h' .. y0 + h'], (x, y) /= (x0, y0) ]
	where
	w' = w - 1
	h' = (h - 1) `div` 2
