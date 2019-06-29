{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module ControlWlS where

import Circuit
import Element
import ControlPartsS

type Control1 = CircuitBuilder (IWire, IWire, IWire, IWire, OWire, OWire, OWire)
type Control0 = CircuitBuilder (IWire, IWire, IWire, OWire)

control1 :: Control1
control1 = do
	(inst, addrCtrl, cntnxt, ckcnt, stout) <- addressSelectLogic
	(incin, incout) <- inc8
	connectWire64 stout incin
	(mmin, mmout) <- microMemory
	connectWire64 stout mmin
	(acin, acout) <- idGate64
	connectWire (mmout, 2, 0) (acin, 2, 0)
	(sl, m0, m1, mo) <- mux2
	connectWire0 ckcnt sl
	connectWire (mmout, 8, 2) (m0, 8, 0)
	return (inst, addrCtrl, cntnxt, m1, acout, incout, mo)

control0 :: Control0
control0 = do
	(instin, _instout) <- idGate64
	(acin, _acout) <- idGate64
	(incin, _incout) <- idGate64
	err <- constGate64 $ Bits 0x1ff
	return (instin, acin, incin, err)

connectControl :: Control1 -> Control0 -> Control0
connectControl c1 cn = do
	(instin, instout) <- idGate64
	(inst, ac, cntnxt, flgsn, acn, cntnxtn, flgs1) <- c1
	(inst', acn', cntnxtn', flgsn') <- cn
	connectWire64 instout inst
	connectWire64 instout inst'
	connectWire64 acn acn'
	connectWire64 cntnxtn cntnxtn'
	connectWire64 flgsn' flgsn
	return (instin, ac, cntnxt, flgs1)

control :: CircuitBuilder (IWire, OWire)
control = do
	(inst, ac, cntnxt, flgs) <- foldr connectControl control0 $ replicate 3 control1
	z <- constGate64 $ Bits 0
	connectWire64 z ac
	connectWire64 z cntnxt
	return (inst, flgs)
