{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Samples.CarryLookahead4 where

import Data.Word

import Circuit
import Circuit.Adornt.Parts
import Samples.CarryLookaheadCommon

raiseGp :: Word8 -> CircuitBuilder (IWire, IWire, OWire, OWire)
raiseGp n = do
	(pin, pout) <- idGate
	(g, p, rg) <- raiseG n
	(p', rp) <- raiseP n
	connectWire64 pout `mapM_` [p, p']
	return (g, pin, rg, rp)

raiseG :: Word8 -> CircuitBuilder (IWire, IWire, OWire)
raiseG n = do
	(gin, gout) <- idGate
	(pin, pout) <- idGate
	(p3, g2, rg2) <- andGate
	(p3', p2', g1', rg1) <- andGate3
	(p3'', p2'', p1'', g0'', rg0) <- andGate4
	(rg3', rg2', rg1', rg0', rg) <- orGate4
	connectWire (gout, m, 3 * n) (rg3', m, 0)
	connectWire (gout, m, 2 * n) (g2, m, 0)
	connectWire (gout, m, 1 * n) (g1', m, 0)
	connectWire (gout, m, 0 * n) (g0'', m, 0)
	connectWire (pout, m, 3 * n) `mapM_` [(p3, m, 0), (p3', m, 0), (p3'', m, 0)]
	connectWire (pout, m, 2 * n) `mapM_` [(p2', m, 0), (p2'', m, 0)]
	connectWire (pout, m, 1 * n) (p1'', m, 0)
	connectWire64 rg2 rg2'
	connectWire64 rg1 rg1'
	connectWire64 rg0 rg0'
	return (gin, pin, rg)
	where m = 64 - 3 * n

raiseP :: Word8 -> CircuitBuilder (IWire, OWire)
raiseP n = do
	(psin, psout) <- idGate
	(p3, p2, p1, p0, rp) <- andGate4
	connectWire (psout, m, 3 * n) (p3, m, 0)
	connectWire (psout, m, 2 * n) (p2, m, 0)
	connectWire (psout, m, 1 * n) (p1, m, 0)
	connectWire (psout, m, 0 * n) (p0, m, 0)
	return (psin, rp)
	where m = 64 - 3 * n

raisingGp :: Word8 -> CircuitBuilder (IWire, IWire, [OWire], [OWire])
raisingGp 0 = do
	(a, b, g1, p1) <- generateGp
	return (a, b, [g1], [p1])
raisingGp n = do
	(gn, pn, rgn, rpn) <- raiseGp n
	(a, b, rga, rpa) <- raisingGp (n `div` 4)
	let	rg : _ = rga
		rp : _ = rpa
	connectWire64 rg gn
	connectWire64 rp pn
	return (a, b, rgn : rga, rpn : rpa)

carries :: CircuitBuilder (IWire, IWire, IWire, OWire, OWire)
carries = do
	(ain, bin, gs, ps) <- raisingGp 16
	cs@(csin, csout) <- idGate
	co <- makeCarries 0 64 gs ps cs
	return (csin, ain, bin, csout, co)

makeCarries :: Word8 -> Word8 -> [OWire] -> [OWire] -> Wire11 -> CircuitBuilder OWire
makeCarries f t [g] [p] (_csin, csout) | f + 1 == t = generateCarry f g p csout
makeCarries f t (g : gs) (p : ps) cs@(csin, csout) = do
	co1 <- makeCarries f l gs ps cs
	co2 <- makeCarries l m gs ps cs
	co3 <- makeCarries m n gs ps cs
	_ <- makeCarries n t gs ps cs
	connectWire (co1, 1, 0) (csin, 1, l)
	connectWire (co2, 1, 0) (csin, 1, m)
	connectWire (co3, 1, 0) (csin, 1, n)
	generateCarry f g p csout
	where
	l = f + 1 * (t - f) `div` 4
	m = f + 2 * (t - f) `div` 4
	n = f + 3 * (t - f) `div` 4
makeCarries _ _ _ _ _ = error "circuit-adornt.Sample.CarryLookahead4.makeCarries _ _ _ _ _"

generateCarry :: Word8 -> OWire -> OWire -> OWire -> CircuitBuilder OWire
generateCarry f g p cs = do
	(p', ci, pci) <- andGate
	(g', pci', co) <- orGate
	connectWire (g, 1, f) (g', 1, 0)
	connectWire (p, 1, f) (p', 1, 0)
	connectWire (cs, 1, f) (ci, 1, 0)
	connectWire0 pci pci'
	return co
