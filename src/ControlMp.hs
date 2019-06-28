{-# LANGUAGE BinaryLiterals #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module ControlMp (
	MainController, mainController, resetMainController,
	mainControllerExternalClockIn, mainControllerInstructionIn,
	mainControllerFlagsOut,
	control, microControl, aluControl ) where

import Circuit
import Element
import Clock
import Memory
import MicroClock
import ControlParts

data MainController = MainController {
	mcMicroClock :: Clock, mcMicroClockFlag :: Register,
	mcExternalClockIn :: IWire,
	mcState :: Register, mcInstIn :: IWire, mcFlagsOut :: OWire }
	deriving Show

mainControllerExternalClockIn :: MainController -> IWire
mainControllerExternalClockIn = mcExternalClockIn

mainControllerInstructionIn :: MainController -> IWire
mainControllerInstructionIn = mcInstIn

mainControllerFlagsOut :: MainController -> OWire
mainControllerFlagsOut = mcFlagsOut

mainController :: CircuitBuilder MainController
mainController = do
	(mcl, mclf, eci, st, inst, fo) <- microControl
	return $ MainController mcl mclf eci st inst fo

resetMainController :: MainController -> Circuit -> Circuit
resetMainController mc cct = let
	cct0 = resetRegister (mcMicroClockFlag mc) cct
	cct1 = resetRegister (mcState mc) cct0
	cct2 = clockOn (mcMicroClock mc) cct1 in
	cct2

control :: CircuitBuilder (Register, IWire, OWire, OWire)
control = do
	(instin, sl, i3, cso, mo) <- addressSelectLogic
	st <- register
	connectWire64 mo (registerInput st)
	(acin, acout) <- microMemory
	connectWire64 (registerOutput st) acin
	connectWire (acout, 2, 0) (sl, 2, 0)
	(incin, incout) <- inc8
	connectWire64 (registerOutput st) incin
	connectWire64 incout i3
	(oin, oout) <- idGate64
	connectWire (acout, 8, 2) (oin, 8, 0)
	return (st, instin, cso, oout)

microControl :: CircuitBuilder (Clock, Register, IWire, Register, IWire, OWire)
microControl = do
	(st, inst, ne, out) <- control
	(mc, mcf, ec) <- microClocked 18 (registerClock st) ne
	return (mc, mcf, ec, st, inst, out)
