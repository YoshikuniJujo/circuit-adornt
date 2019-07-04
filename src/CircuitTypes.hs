{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module CircuitTypes where

import Prelude as P

import Control.Monad.State
import Data.Bits ((.&.), (.|.))
import Data.Word
import Data.Map
import Data.Array as A

import qualified Data.Bits as B
import qualified Data.List as L

import Tools

newtype Bits = Bits Word64 deriving (Show, Eq)

notBits :: Bits -> Bits
notBits (Bits w) = Bits $ B.complement w

andBits, orBits :: Bits -> Bits -> Bits
andBits (Bits v) (Bits w) = Bits $ v .&. w
orBits (Bits v) (Bits w) = Bits $ v .|. w

testBit :: Bits -> Word8 -> Bool
testBit (Bits w) i = B.testBit w $ fromIntegral i

type FromOWire = ((BitLen, BitPosOut), (BitLen, BitPosIn))

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

newtype IWire = IWire Word32 deriving (Show, Eq, Ord)
data OWire = OWire Word32 (Maybe IWire) deriving (Show, Eq, Ord)

triIWire :: OWire -> Maybe IWire
triIWire (OWire _ mi) = mi

type BitLen = Word8
type BitPosIn = Word8
type BitPosOut = Word8

bitsToWord :: Bits -> Word64
bitsToWord (Bits w) = w

wordToBits :: Word64 -> Bits
wordToBits = Bits

data Circuit = Circuit {
	cctGate :: Map OWire BasicGate,
	cctWireConn :: Map IWire [(OWire, FromOWire)],
	cctWireStt :: Map IWire [Bits] }
	deriving Show

type CircuitBuilder = State CBState

makeIWire :: CircuitBuilder IWire
makeIWire = IWire <$> getModify cbsWireNum sccWireNum

makeOWire :: CircuitBuilder OWire
makeOWire = (`OWire` Nothing) <$> getModify cbsWireNum sccWireNum

makeOWireTri :: IWire -> CircuitBuilder OWire
makeOWireTri i = (`OWire` Just i) <$> getModify cbsWireNum sccWireNum

sccWireNum :: CBState -> CBState
sccWireNum cbs = cbs { cbsWireNum = cbsWireNum cbs + 1 }

data CBState = CBState {
	cbsWireNum :: Word32,
	cbsGate :: Map OWire BasicGate,
	cbsWireConn :: Map IWire [(OWire, FromOWire)],
	cbsDelay :: Map IWire Word8 }
	deriving Show

initCBState :: CBState
initCBState = CBState {
	cbsWireNum = 0, cbsGate = empty, cbsWireConn = empty, cbsDelay = empty }

data BasicGate
	= ConstGate Bits
	| IdGate IWire | NotGate IWire
	| AndGate IWire IWire | OrGate IWire IWire
	deriving Show

gateWires :: BasicGate -> [IWire]
gateWires (ConstGate _) = []
gateWires (IdGate i) = [i]
gateWires (NotGate i) = [i]
gateWires (AndGate a b) = [a, b]
gateWires (OrGate a b) = [a, b]
