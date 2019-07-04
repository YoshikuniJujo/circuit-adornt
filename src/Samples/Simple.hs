{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}

module Samples.Simple () where

import Control.Arrow
import Control.Monad
import Data.Foldable
import Data.List
import Data.Bool
import Data.Word

import qualified Data.Bits as B

import Circuit
import Tools

xorGate :: CircuitBuilder (IWire, IWire, OWire)
xorGate = do
	(ain, aout) <- idGate
	(bin, bout) <- idGate
	(aa, ab, ao) <- andGate
	(oa, ob, oo) <- orGate
	connectWire64 aout aa
	connectWire64 bout ab
	connectWire64 aout oa
	connectWire64 bout ob
	(ni, no) <- notGate
	(na, o, r) <- andGate
	connectWire64 ao ni
	connectWire64 no na
	connectWire64 oo o
	return (ain, bin, r)

sampleTri :: CircuitBuilder (IWire, IWire, IWire, IWire, OWire)
sampleTri = do
	(a1, b1, o1) <- triGate
	(a2, b2, o2) <- triGate
	(oin, oout) <- idGate
	connectWire64 o1 oin
	connectWire64 o2 oin
	return (a1, b1, a2, b2, oout)

decoder :: Word16 -> CircuitBuilder ([IWire], [OWire])
decoder n = do
	(is, ois) <- unzip <$> fromIntegral m `replicateM` idGate
	(ias, oas) <- unzip <$> fromIntegral n `replicateM` multiAndGate m
	zipWithM_ ((sequence_ .)
		. flip (zipWith3 id) ois) (binary (inverse, obverse) m) ias
	return (is, oas)
	where m = log2 n

decoder' :: Word8 -> CircuitBuilder (IWire, OWire)
decoder' n = do
	(iin, iout) <- idGate
	(oin, oout) <- idGate
	(is, os) <- decoder $ fromIntegral n
	for_ (zip [0 ..] is) $ \(i, ip) -> connectWire (iout, 1, i) (ip, 1, 0)
	for_ (zip [0 ..] os) $ \(i, op) -> connectWire (op, 1, 0) (oin, 1, i)
	return (iin, oout)

inverse, obverse :: OWire -> IWire -> CircuitBuilder ()
inverse o i = do
	(ni, no) <- notGate
	zipWithM_ connectWire64 [o, no] [ni, i]
obverse = connectWire64

multiAndGate, multiOrGate :: Word16 -> CircuitBuilder ([IWire], OWire)
multiAndGate = multiple andGate
multiOrGate = multiple orGate

multiple :: CircuitBuilder (IWire, IWire, OWire) ->
	Word16 -> CircuitBuilder ([IWire], OWire)
multiple _ n | n < 0 = error "circuit-adornt.Sample.Simple.multiple _ n | n < 0"
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

connectWire0_64 :: OWire -> IWire -> CircuitBuilder ()
connectWire0_64 o i = connectWire (o, 1, 0) (i, 64, 0)

data Bit = O | I deriving (Show, Eq)

numToBits :: B.Bits n => Word8 -> n -> [Bit]
numToBits c n = map (bool O I . B.testBit n) [0 .. fromIntegral c - 1]

pla1 :: [OWire] -> [([Bit], Bit)] -> CircuitBuilder OWire
pla1 iouts tbl1 = do
	fbs <- frbk0 `mapM` iouts
	let	ws = map (zipWith bitIndex fbs) ons
	r <- allOr =<< mapM allAnd ws
	return r
	where
	ons = fst <$> filter ((== I) . snd) tbl1

bitIndex :: (a, a) -> Bit -> a
bitIndex (x, _) O = x
bitIndex (_, y) I = y

frbk0 :: OWire -> CircuitBuilder (OWire, OWire)
frbk0 o = do
	(ni, no) <- notGate
	connectWire0 o ni
	return  (no, o)

allAnd, allOr :: [OWire] -> CircuitBuilder OWire
allAnd os = do
	(as, o) <- multiAndGate . fromIntegral $ length os
	zipWithM_ connectWire64 os as
	return o

allOr os = do
	(xs, o) <- multiOrGate . fromIntegral $ length os
	zipWithM_ connectWire64 os xs
	return o

plaGen :: Word8 -> [([Bit], [Bit])] -> CircuitBuilder ([IWire], [OWire])
plaGen n_ tbl = do
	(iins, iouts) <- unzip <$> n `replicateM` idGate
	rs <- pla1 iouts `mapM` trSep tbl
	return (iins, rs)
	where n = fromIntegral n_

trSep :: [(a, [b])] -> [[(a, b)]]
trSep = transpose . map sepSnd

sepSnd :: (a, [b]) -> [(a, b)]
sepSnd (x, ys) = zip (repeat x) ys

pla8 :: [(Word8, Word8)] -> CircuitBuilder (IWire, OWire)
pla8 tbl_ = do
	(iin, iout) <- idGate
	(iss, outs) <- plaGen 8 tbl
	(oin, oout) <- idGate
	connectWireOutToIns iout iss
	connectWireOutsToIn outs oin
	return (iin, oout)
	where tbl = (numToBits 8 *** numToBits 8) <$> tbl_

connectWireOutToIns :: OWire -> [IWire] -> CircuitBuilder ()
connectWireOutToIns o is = zipWithM_ (\n i -> connectWire (o, 1, n) (i, 1, 0)) [0 ..] is

connectWireOutsToIn :: [OWire] -> IWire -> CircuitBuilder ()
connectWireOutsToIn os i = zipWithM_ (\n o -> connectWire (o, 1, 0) (i, 1, n)) [0 ..] os
