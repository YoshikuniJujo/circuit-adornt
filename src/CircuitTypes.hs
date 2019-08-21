{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module CircuitTypes (
	CBState(..), OWire(..), BasicGate(..), IWire, CircuitBuilder, Bits(..), BitPosIn, BitPosOut, BitLen,
	Circuit(..), FromOWire, makeIWire, makeOWireTri, makeOWire, andBits, orBits, fromOWire, testBit, notBits,
	triIWire, gateWires, initCBState, bitsToWord, wordToBits
	) where

import Prelude as P

import Data.Bits ((.&.), (.|.))
import Data.Word
import Data.Map
import Data.Array as A

import qualified Data.Bits as B
import qualified Data.List as L

import Circuit.Adornt.Builder

newtype Bits = Bits Word64 deriving (Show, Eq, Ord)

notBits :: Bits -> Bits
notBits (Bits w) = Bits $ B.complement w

andBits, orBits :: Bits -> Bits -> Bits
andBits (Bits v) (Bits w) = Bits $ v .&. w
orBits (Bits v) (Bits w) = Bits $ v .|. w

testBit :: Bits -> Word8 -> Bool
testBit (Bits w) i = B.testBit w $ fromIntegral i

fromOWire :: FromOWire -> Bits -> Bits -> Bits
fromOWire ((blo, bpo_), (bli, bpi_)) (Bits bo) (Bits bi)
	| blo == bli = Bits $ bo'' .|. bi'
	| otherwise = Bits $ bo' .|. bi'
	where
	bo'' = (bo `B.shiftR` bpo) `B.shiftL` bpi .&. maskBits bli bpi_
	bo' = (bo `B.shiftR` bpo) `cycleBits`
		blo `B.shiftL` bpi .&. maskBits bli bpi_
	bi' = bi .&. windowBits bli bpi_
	[bpo, bpi] = fromIntegral <$> [bpo_, bpi_]

cycleBits :: Word64 -> Word8 -> Word64
cycleBits _ 0 = error "cycleBits n c: c should not be 0"
cycleBits n c = cb $ 64 `div` c + signum (64 `mod` c)
	where
	cb i | i < 1 = B.zeroBits
	cb i = cb (i - 1) `B.shiftL` fromIntegral c .|. n .&. maskBits c 0

maskBits, maskBits', windowBits :: BitLen -> BitPosOut -> Word64
windowBits ln ps = B.complement $ maskBits ln ps
maskBits' ln ps =
	L.foldl' B.setBit B.zeroBits $ fromIntegral <$> [ps .. ps + ln - 1]
maskBits ln ps = maskBitsList A.! (fromIntegral ln * 64 + fromIntegral ps)

maskBitsList :: Array Int Word64
maskBitsList = listArray (0, 4159) $ P.concatMap (\ln -> P.map (maskBits' ln) [0 .. 63]) [0 .. 64]

bitsToWord :: Bits -> Word64
bitsToWord (Bits w) = w

wordToBits :: Word64 -> Bits
wordToBits = Bits

data Circuit = Circuit {
	cctGate :: Map OWire BasicGate,
	cctWireConn :: Map IWire [(OWire, FromOWire)],
	cctWireStt :: Map IWire [Bits] }
--	deriving Show
