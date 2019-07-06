{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Element where

import Control.Arrow
import Control.Monad
import Data.Foldable
import Data.Word

import Circuit
import Tools

norGate :: CircuitBuilder Wire21
norGate = do
	(oa, ob, oo) <- orGate
	(ni, no) <- notGate
	connectWire64 oo ni
	return (oa, ob, no)

xorGate :: CircuitBuilder Wire21
xorGate = do
	(ain, aout) <- idGate
	(bin, bout) <- idGate
	(aa, ab, ao) <- andGate
	(na, nb, no) <- norGate
	(ad, nor, xo) <- norGate
	connectWire64 aout `mapM_` [aa, na]
	connectWire64 bout `mapM_` [ab, nb]
	connectWire64 ao ad
	connectWire64 no nor
	return (ain, bin, xo)

andNotBGate :: CircuitBuilder Wire21
andNotBGate = do
	(ni, no) <- notGate
	(aa, ab, ao) <- andGate
	connectWire64 no ab
	return (aa, ni, ao)

multiple :: CircuitBuilder Wire21 -> Word16 -> CircuitBuilder ([IWire], OWire)
multiple _ n | n < 0 = error "circuit-adornt.Element.multiple _ n | n < 0"
multiple _ 0 = ([] ,) <$> constGate (Bits 0)
multiple _ 1 = first (: []) <$> idGate
multiple g 2 = (\(a, b, o) -> ([a, b], o)) <$> g
multiple g n = do
	(is1, o1) <- multiple g (n `div` 2)
	(is2, o2) <- multiple g (n - n `div` 2)
	(a, b, o) <- g
	connectWire64 o1 a
	connectWire64 o2 b
	return (is1 ++ is2, o)

multi3 :: CircuitBuilder Wire21 -> CircuitBuilder Wire31
multi3 g = do
	(is, o) <- multiple g 3
	let	(i0, i1, i2) = listToTuple3 is
	return (i0, i1, i2, o)

multi4 :: CircuitBuilder Wire21 -> CircuitBuilder Wire41
multi4 g = do
	(is, o) <- multiple g 4
	let	(i0, i1, i2, i3) = listToTuple4 is
	return (i0, i1, i2, i3, o)

multiAndGate, multiOrGate, multiXorGate :: Word16 -> CircuitBuilder ([IWire], OWire)
[multiAndGate, multiOrGate, multiXorGate] = multiple <$> [andGate, orGate, xorGate]

andGate3, orGate3, xorGate3 :: CircuitBuilder Wire31
[andGate3, orGate3, xorGate3] = multi3 <$> [andGate, orGate, xorGate]

andGate4, orGate4, xorGate4 :: CircuitBuilder Wire41
[andGate4, orGate4, xorGate4] = multi4 <$> [andGate, orGate, xorGate]

decoder :: Word16 -> CircuitBuilder ([IWire], [OWire])
decoder n = do
	(is, ois) <- unzip <$> fromIntegral m `replicateM` idGate
	(ias, oas) <- unzip <$> fromIntegral n `replicateM` multiAndGate m
	zipWithM_ ((sequence_ .)
		. flip (zipWith3 id) ois) (binary (inverse, obverse) m) ias
	return (is, oas)
	where m = log2 n

inverse, obverse :: OWire -> IWire -> CircuitBuilder ()
inverse o i = do
	(ni, no) <- notGate
	zipWithM_ connectWire64 [o, no] [ni, i]
obverse = connectWire64

mux2 :: CircuitBuilder (IWire, IWire, IWire, OWire)
mux2 = do
	(sl, is, o) <- multiplexer 2
	let	(i0, i1) = listToTuple2 is
	return (sl, i0, i1, o)

mux3 :: CircuitBuilder (IWire, IWire, IWire, IWire, OWire)
mux3 = do
	(sl, is, o) <- multiplexer 3
	let	(i0, i1, i2) = listToTuple3 is
	return (sl, i0, i1, i2, o)

mux4 :: CircuitBuilder (IWire, IWire, IWire, IWire, IWire, OWire)
mux4 = do
	(sl, is, o) <- multiplexer 4
	let	(i0, i1, i2, i3) = listToTuple4 is
	return (sl, i0, i1, i2, i3, o)

multiplexer :: Word16 -> CircuitBuilder (IWire, [IWire], OWire)
multiplexer n = do
	(slin, slout) <- idGate
	(dins, douts) <- decoder n
	for_ (zip [0 ..] dins)
		$ \(i, din) -> connectWire (slout, 1, i) (din, 1, 0)
	(as, bs, os) <- unzip3 <$> fromIntegral n `replicateM` andGate
	(ois, oo) <- multiOrGate n
	zipWithM_ connectWire0_64 douts as
	zipWithM_ connectWire64 os ois
	return (slin, bs, oo)

testZero :: CircuitBuilder (IWire, OWire)
testZero = do
	(iin, iout) <- idGate
	(ois, oo) <- multiOrGate 64
	(ni, no) <- notGate
	for_ (zip [0 ..] ois) $ \(i, oi) -> connectWire (iout, 1, i) (oi, 1, 0)
	connectWire0 oo ni
	(oin, oout) <- idGate
	connectWire0 no oin
	return (iin, oout)
