{-# LANGUAGE BinaryLiterals #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Instructions.MakeInstruction (Reg(..), Inst(..), encodeInst) where

import Data.Bits
import Data.Word
import Data.Int

data Reg = Reg Word8 deriving Show
type Imm = Word16

data Inst
	= Load Reg Imm Reg | Store Reg Imm Reg
	| Add Reg Reg Reg | Sub Reg Reg Reg
	| And Reg Reg Reg | Or Reg Reg Reg
	| Beq Reg Reg Offset | Nop
	deriving Show

encodeInst :: Inst -> Word64
encodeInst (Load rd imm rs1) = fromIntegral . packLoad $ LLoad rd imm rs1
encodeInst (Store rs2 imm rs1) = fromIntegral . packStore $ SStore rs2 imm rs1
encodeInst (Add rd rs1 rs2) = fromIntegral . packRtypeInst $ AAdd rd rs1 rs2
encodeInst (Sub rd rs1 rs2) = fromIntegral . packRtypeInst $ SSub rd rs1 rs2
encodeInst (And rd rs1 rs2) = fromIntegral . packRtypeInst $ AAnd rd rs1 rs2
encodeInst (Or rd rs1 rs2) = fromIntegral . packRtypeInst $ OOr rd rs1 rs2
encodeInst (Beq rs1 rs2 os) = fromIntegral . packSbtype . beqToWords $ BBeq rs1 rs2 os
encodeInst Nop = fromIntegral . packSbtype $ beqToWords NNop

data Rtype = AAdd Reg Reg Reg | SSub Reg Reg Reg | AAnd Reg Reg Reg | OOr Reg Reg Reg
	deriving Show

packRtypeInst :: Rtype -> Word32
packRtypeInst (AAdd (Reg rd) (Reg rs1) (Reg rs2)) =
	packRType [0, rs2, rs1, 0, rd, 0b0110011]
packRtypeInst (SSub (Reg rd) (Reg rs1) (Reg rs2)) =
	packRType [0b0100000, rs2, rs1, 0, rd, 0b0110011]
packRtypeInst (AAnd (Reg rd) (Reg rs1) (Reg rs2)) =
	packRType [0b0000000, rs2, rs1, 0b111, rd, 0b0110011]
packRtypeInst (OOr (Reg rd) (Reg rs1) (Reg rs2)) =
	packRType [0b0000000, rs2, rs1, 0b110, rd, 0b0110011]

-- R type: 7 5 5 3 5 7

packRType :: [Word8] -> Word32
packRType ws = f7 .|. r2 .|. r1 .|. f3 .|. rd .|. op
	where
	[f7_, r2_, r1_, f3_, rd_, op] = fromIntegral <$> ws
	f7 = f7_ `shiftL` 25; r2 = r2_ `shiftL` 20; r1 = r1_ `shiftL` 15
	f3 = f3_ `shiftL` 12; rd = rd_ `shiftL` 7

data Load = LLoad Reg Imm Reg deriving Show

packLoad :: Load -> Word32
packLoad (LLoad (Reg rd) imm_ (Reg r1)) = packIType [imm1, imm0, r1, 3, rd, 3]
	where
	imm1 = fromIntegral $ imm_ `shiftR` 8
	imm0 = fromIntegral imm_

packIType :: [Word8] -> Word32
packIType ws = imm1 .|. imm0 .|. r1 .|. f3 .|. rd .|. op
	where
	[imm1_, imm0_, r1_, f3_, rd_, op] = fromIntegral <$> ws
	imm1 = imm1_ `shiftL` 28
	imm0 = imm0_ `shiftL` 20
	r1 = r1_ `shiftL` 15
	f3 = f3_ `shiftL` 12
	rd = rd_ `shiftL` 7

data Store = SStore Reg Imm Reg deriving Show

packStore :: Store -> Word32
packStore (SStore (Reg rs2) imm (Reg rs1)) =
	packStype [
		fromIntegral $ imm `shiftR` 5, rs2, rs1, 3,
		fromIntegral $ imm .&. 0x1f, 35 ]

packStype :: [Word8] -> Word32
packStype ws = imm11_5 .|. r2 .|. r1 .|. f3 .|. imm4_0 .|. op
	where
	[imm11_5_, r2_, r1_, f3_, imm4_0_, op] = fromIntegral <$> ws
	imm11_5 = imm11_5_ `shiftL` 25
	r2 = r2_ `shiftL` 20; r1 = r1_ `shiftL` 15
	f3 = f3_ `shiftL` 12; imm4_0 = imm4_0_ `shiftL` 7

type Offset = Int16

data Beq = BBeq Reg Reg Offset | NNop deriving Show

beqToWords :: Beq -> [Word8]
beqToWords (BBeq (Reg rs1) (Reg rs2) imm) =
	[0x67, fromIntegral imm1, 0, rs1, rs2, fromIntegral imm2]
	where
	imm1 = imm .&. 0x1e .|. imm `shiftR` 11 .&. 0x01
	imm2 = imm `shiftR` 5 .&. 0x3f .|. imm `shiftR` 6 .&. 0x40
beqToWords NNop = [0x13, 0, 0, 0, 0, 0]

packSbtype :: [Word8] -> Word32
packSbtype ws@[_op, _imm1, _f_, _rs1, _rs2, _imm2] = 
	op .|. imm1 `shiftL` 7 .|. f3 `shiftL` 12 .|. rs1 `shiftL` 15 .|.
	rs2 `shiftL` 20 .|. imm2 `shiftL` 25
	where
	[op, imm1, f3, rs1, rs2, imm2] = fromIntegral <$> ws
packSbtype _ = error "Oops!"
