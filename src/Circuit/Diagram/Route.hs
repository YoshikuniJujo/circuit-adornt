{-# LANGUAGE TupleSections, TypeFamilies #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Circuit.Diagram.Route (route) where

import Prelude as P

import Data.Int
import Data.Map.Strict

import Circuit.Diagram.CircuitDiagram
import AStar.AStar

route :: (Int8, Int8) -> (Int8, Int8) -> CircuitDiagram -> Maybe [(Int8, Int8)]
route s g cd = astar CircuitDiagramAStar {
	cdaStart = s, cdaEnd = g, cdaCircuitDiagram = cd }

data CircuitDiagramAStar = CircuitDiagramAStar {
	cdaStart :: (Int8, Int8),
	cdaEnd :: (Int8, Int8),
	cdaCircuitDiagram :: CircuitDiagram }

instance AStar CircuitDiagramAStar where
	type AStarNode CircuitDiagramAStar = (Int8, Int8)
	startNode = cdaStart
	isEndNode cda = (== cdaEnd cda)
	nextNodes cda = ((, 1) <$>) . nextPos cda
	distToEnd = distanceToEnd

data VH = Vertical | Horizontal deriving Show

nextPos :: CircuitDiagramAStar -> (Int8, Int8) -> [(Int8, Int8)]
nextPos cda (x, y) =
	P.filter (checkPos cda Vertical) [(x, y - 1), (x, y + 1)] ++
	P.filter (checkPos cda Horizontal) [(x - 1, y), (x + 1, y)]

checkPos :: CircuitDiagramAStar -> VH -> (Int8, Int8) -> Bool
checkPos cda vh ps
	| cdaEnd cda == ps = True
	| otherwise = case (cdDiagram cd !? ps, vh) of
		(Nothing, _) -> True
		(Just HLineD, Vertical) -> True
		(Just VLineD, Horizontal) -> True
		_ -> False
	where
	cd = cdaCircuitDiagram cda

distanceToEnd :: CircuitDiagramAStar -> (Int8, Int8) -> Dist
distanceToEnd cda (x, y) = fromIntegral $ abs (xg - x) + abs (yg - y)
	where
	(xg, yg) = cdaEnd cda
