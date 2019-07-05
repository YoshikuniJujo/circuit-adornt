{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module CarryLookahead2 where

import Control.Monad
import Data.Word

import Circuit

generateGp :: CircuitBuilder (IWire, IWire, OWire, OWire)
generateGp = do
	(ain, aout) <- idGate
	(bin, bout) <- idGate
	(aa, ab, g) <- andGate
	(oa, ob, p) <- orGate
	zipWithM_ connectWire64 [aout, bout, aout, bout] [aa, ab, oa, ob]
	return (ain, bin, g, p)

raiseGp :: Word8 -> CircuitBuilder (IWire, IWire, OWire, OWire)
raiseGp n = do
	(gin, gout) <- idGate
	(pin, pout) <- idGate
	(p1g, g0, p1g0) <- andGate
	(g1, p1g0', rgout) <- orGate
	(p1p, p0, rpout) <- andGate
	connectWire (pout, m, n) (p1g, m, 0)
	connectWire (gout, m, 0) (g0, m, 0)
	connectWire (gout, m, n) (g1, m, 0)
	connectWire (p1g0, m, 0) (p1g0', m, 0)
	connectWire (pout, m, n) (p1p, m, 0)
	connectWire (pout, m, 0) (p0, m, 0)
	return (gin, pin, rgout, rpout)
	where m = 64 - n

raisingGp :: Word8 -> OWire -> OWire -> CircuitBuilder ([OWire], [OWire])
raisingGp 0 a b = do
	(a', b', g1, p1) <- generateGp
	zipWithM_ connectWire64 [a, b] [a', b']
	return ([g1], [p1])
raisingGp n a b = do
	(gn, pn, rgn, rpn) <- raiseGp n
	(rga, rpa) <- raisingGp (n `div` 2) a b
	let	rg : _ = rga
		rp : _ = rpa
	connectWire64 rg gn
	connectWire64 rp pn
	return (rgn : rga, rpn : rpa)

carries :: OWire -> OWire -> OWire -> CircuitBuilder (OWire, OWire)
carries ci a b = do
	(gs, ps) <- raisingGp 32 a b
	cs@(csin, csout) <- idGate
	co <- makeCarries 0 64 gs ps cs
	connectWire (ci, 1, 0) (csin, 1, 0)
	return (csout, co)

makeCarries :: Word8 -> Word8 -> [OWire] -> [OWire] -> (IWire, OWire) -> CircuitBuilder OWire
makeCarries f t [g] [p] (_csin, csout) | f + 1 == t = generateCarry f g p csout
makeCarries f t (g : gs) (p : ps) cs@(csin, csout) = do
	co <- makeCarries f m gs ps cs
	_ <- makeCarries m t gs ps cs
	connectWire (co, 1, 0) (csin, 1, m)
	generateCarry f g p csout
	where m = (t - f) `div` 2 + f
makeCarries _ _ _ _ _ = error "circuit.adornt.Sample.CarryLookaphead2.makeCarries _ _ _ _"

generateCarry :: Word8 -> OWire -> OWire -> OWire -> CircuitBuilder OWire
generateCarry f g p cs = do
	(p', ci, pci) <- andGate
	(g', pci', co) <- orGate
	connectWire (g, 1, f) (g', 1, 0)
	connectWire (p, 1, f) (p', 1, 0)
	connectWire (cs, 1, f) (ci, 1, 0)
	connectWire0 pci pci'
	return co

testCarries :: CircuitBuilder (IWire, IWire, IWire, OWire, OWire)
testCarries = do
	(cin, ciout) <- idGate
	(ain, aout) <- idGate
	(bin, bout) <- idGate
	(cs, co) <- carries ciout aout bout
	return (cin, ain, bin, cs, co)
