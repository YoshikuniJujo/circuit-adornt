{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Element where

import Control.Arrow
import Data.Word

import Circuit
import Tools

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

multiAndGate, multiOrGate :: Word16 -> CircuitBuilder ([IWire], OWire)
[multiAndGate, multiOrGate] = multiple <$> [andGate, orGate]

andGate3, orGate3 :: CircuitBuilder Wire31
[andGate3, orGate3] = multi3 <$> [andGate, orGate]

andGate4, orGate4 :: CircuitBuilder Wire41
[andGate4, orGate4] = multi4 <$> [andGate, orGate]
