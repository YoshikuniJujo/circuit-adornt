{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Element (module Element, module Circuit.Adornt.Parts) where

import Control.Arrow
import Control.Monad
import Data.Foldable
import Data.List
import Data.Bool
import Data.Word

import Circuit.Adornt.Parts

import Circuit

import qualified Data.Bits as B

testZero :: CircuitBuilder (IWire, OWire)
testZero = do
	(iin, iout) <- idGate
	(ois, oo) <- multiple orGate 64
	(ni, no) <- notGate
	for_ (zip [0 ..] ois) $ \(i, oi) -> connectWire (iout, 1, i) (oi, 1, 0)
	connectWire0 oo ni
	(oin, oout) <- idGate
	connectWire0 no oin
	cz <- constGate $ Bits 0
	connectWire (cz, 63, 0) (oin, 63, 1)
	return (iin, oout)

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

plaGen :: Word8 -> [([Bit], [Bit])] -> CircuitBuilder ([IWire], [OWire])
plaGen n_ tbl = do
	(iins, iouts) <- unzip <$> n `replicateM` idGate
	rs <- pla1 iouts `mapM` trSep tbl
	return (iins, rs)
	where n = fromIntegral n_

pla8 :: [(Word8, Word8)] -> CircuitBuilder (IWire, OWire)
pla8 tbl_ = do
	(iin, iout) <- idGate
	(iss, outs) <- plaGen 8 tbl
	(oin, oout) <- idGate
	connectWireOutToIns iout iss
	connectWireOutsToIn outs oin
	z <- constGate $ Bits 0
	connectWire (z, 56, 0) (oin, 56, 8)
	return (iin, oout)
	where tbl = (numToBits 8 *** numToBits 8) <$> tbl_

connectWireOutToIns :: OWire -> [IWire] -> CircuitBuilder ()
connectWireOutToIns o is = zipWithM_ (\n i -> connectWire (o, 1, n) (i, 1, 0)) [0 ..] is

connectWireOutsToIn :: [OWire] -> IWire -> CircuitBuilder ()
connectWireOutsToIn os i = zipWithM_ (\n o -> connectWire (o, 1, 0) (i, 1, n)) [0 ..] os

allAnd, allOr :: [OWire] -> CircuitBuilder OWire
allAnd os = do
	(as, o) <- multiple andGate . fromIntegral $ length os
	zipWithM_ connectWire64 os as
	return o

allOr os = do
	(xs, o) <- multiple orGate . fromIntegral $ length os
	zipWithM_ connectWire64 os xs
	return o

trSep :: [(a, [b])] -> [[(a, b)]]
trSep = transpose . map sepSnd

sepSnd :: (a, [b]) -> [(a, b)]
sepSnd (x, ys) = zip (repeat x) ys
