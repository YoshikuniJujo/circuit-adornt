{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import Circuit
import Memory
import TrySampleCpuWlS

main :: IO ()
main = print . take 718
	$ peekMultOWires (rrfAllOutputs trySingleCycleRrfWl)
		<$> iterate step trySingleCycleCctWl
