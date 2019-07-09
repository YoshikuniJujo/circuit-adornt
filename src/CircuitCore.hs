{-# LANGUAGE TupleSections, MonadComprehensions #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module CircuitCore (
	Circuit, makeCircuit, step,
	CircuitBuilder,
	IWire, OWire, Bits(..), BitLen, BitPosIn, BitPosOut,
	constGate, idGate, notGate, andGate, orGate, triGate,
	connectWire, delay,
	setBits, peekOWire, bitsToWord, wordToBits
	) where

import Prelude
import qualified Prelude as P

import Control.Arrow
import Control.Monad.State
import Data.Maybe
import Data.Word
import Data.Map

import qualified Data.Map as M

import CircuitTypes
import Tools

makeCircuit :: [Bits] -> CircuitBuilder a -> (a, Circuit)
makeCircuit ibs cb = (x ,) $ Circuit {
	cctGate = gs, cctWireConn = wc,
	cctWireStt = fromList $ (makeWireState dm `mapM` (iwsfg ++ iwsfo)) `evalState` ibs }
	where
	(x, CBState { cbsGate = gs, cbsWireConn = wc, cbsDelay = dm }) =
		cb `runState` initCBState
	iwsfg = gateWires =<< elems gs
	iwsfo = catMaybes $ triIWire <$> keys gs

makeWireState :: Map IWire Word8 -> IWire -> State [Bits] (IWire, [Bits])
makeWireState dm iw = (iw ,) <$> pop (fromMaybe 1 $ dm !? iw)

pop :: Integral n => n -> State [a] [a]
pop n = uncurry (<*) . (return *** put) . P.splitAt (fromIntegral n) =<< get

step :: Circuit -> Circuit
step cct@Circuit { cctGate = gs, cctWireConn = wc, cctWireStt = wst } = let
	ows = M.map (checkOWire wst) gs in
	cct { cctWireStt = mapWithKey (nextIWire wst wc ows) wst }

setBits :: IWire -> Bits -> Circuit -> Circuit
setBits w bs c = c { cctWireStt = insertPush w bs $ cctWireStt c }

nextIWire :: Map IWire [Bits] -> Map IWire [(OWire, FromOWire)] -> Map OWire Bits -> IWire -> [Bits] -> [Bits]
nextIWire wst wc ows iw oba@(_ : obs) =
	(obs ++) . (: []) . fromMaybe ob $ do
		fows <- wc !? iw
		return $ P.foldr
			(uncurry . flip $ nextIWireFromOWire wst ows) ob fows
	where ob = last oba
nextIWire _ _ _ _ [] = error "circuit-adornt.CircuitCore.nextIWire _ _ _ []"

nextIWireFromOWire :: Map IWire [Bits] -> Map OWire Bits -> FromOWire -> OWire -> Bits -> Bits
nextIWireFromOWire wst ows fow ow@(OWire _ msw_) b = fromMaybe b $ do
	case msw_ of
		Just sw_ -> do
			sw <- wst !!? sw_
			guard $ testBit sw 0
		Nothing -> return ()
	return $ nextIWireFromOWire' ows fow ow b

nextIWireFromOWire' :: Map OWire Bits -> FromOWire -> OWire -> Bits -> Bits
nextIWireFromOWire' ows fow ow b = fromMaybe b $ do
	owb <- ows !? ow
	return $ fromOWire fow owb b

peekOWire :: OWire -> Circuit -> Bits
peekOWire w Circuit { cctGate = gs, cctWireStt = wst } =
	fromJust $ calcGate wst <$> (gs !? w)

calcGate, checkOWire :: Map IWire [Bits] -> BasicGate -> Bits
calcGate _ (ConstGate bs) = bs
calcGate wst (IdGate i) = fromMaybe (Bits 0) $ wst !!? i
calcGate wst (NotGate i) = maybe (Bits 1) notBits $ wst !!? i
calcGate wst (AndGate a_ b_) =
	fromMaybe (Bits 0) $ [ andBits a b | a <- wst !!? a_, b <- wst !!? b_ ]
calcGate wst (OrGate a_ b_) =
	fromMaybe (Bits 0) $ [ orBits a b | a <- wst !!? a_, b <- wst !!? b_  ]

checkOWire = calcGate

connectWire :: (OWire, BitLen, BitPosOut) ->
	(IWire, BitLen, BitPosIn) -> CircuitBuilder ()
connectWire (_, obl, obp) (_, ibl, ibp)
	| obl <= 0 || ibl <= 0 =
		error "connectWire: length should be larger than 0"
	| obp < 0 || ibp < 0 =
		error "connectWire: position should be larger than or equal to 0"
	| obl + obp > 64 || ibl + ibp > 64 = error
		$ "connectWire: length + position should be less then or equal to 64\n" ++
			"\tobl: " ++ show obl ++ " obp: " ++ show obp ++ " ibl: " ++ show ibl ++ " ibp: " ++ show ibp
connectWire (o, obl, obp) (i, ibl, ibp) =
	modify $ insConn ((obl, obp), (ibl, ibp))
	where insConn f cbs = cbs {
		cbsWireConn =
			insert i ((o, f) : indexOrEmpty (cbsWireConn cbs) i)
				$ cbsWireConn cbs }

delay :: IWire -> Word8 -> CircuitBuilder ()
delay _ 0 = error "0 delay is not permitted"
delay iw n = modify insDelay
	where insDelay cbs = cbs { cbsDelay = insert iw n $ cbsDelay cbs }

constGate :: Bits -> CircuitBuilder OWire
constGate bs = do
	o <- makeOWire
	modify $ insGate (ConstGate bs) o
	return o

idGate, notGate :: CircuitBuilder (IWire, OWire)
idGate = do
	io@(i, o) <- (,) <$> makeIWire <*> makeOWire
	modify $ insGate (IdGate i) o
	return io

notGate = do
	io@(i, o) <- (,) <$> makeIWire <*> makeOWire
	modify $ insGate (NotGate i) o
	return io

andGate, orGate :: CircuitBuilder (IWire, IWire, OWire)
andGate = do
	abo@(a, b, o) <- (,,) <$> makeIWire <*> makeIWire <*> makeOWire
	modify $ insGate (AndGate a b) o
	return abo
orGate = do
	abo@(a, b, o) <- (,,) <$> makeIWire <*> makeIWire <*> makeOWire
	modify $ insGate (OrGate a b) o
	return abo

triGate :: CircuitBuilder (IWire, IWire, OWire)
triGate = do
	(a, b) <- (,) <$> makeIWire <*> makeIWire
	o <- makeOWireTri b
	modify $ insGate (IdGate a) o
	return (a, b, o)

insGate :: BasicGate -> OWire -> CBState -> CBState
insGate g o cbs = cbs { cbsGate = insert o g $ cbsGate cbs }
