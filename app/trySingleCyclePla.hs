{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import Circuit
import Memory
import TrySampleCpuPla

main :: IO ()
main = print . take 629
	$ peekMultOWires (rrfAllOutputs trySingleCyclePlaRrf)
		<$> iterate step trySingleCyclePlaCct
