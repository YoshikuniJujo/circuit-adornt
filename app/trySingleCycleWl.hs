{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import Circuit
import Memory
import TrySampleCpuWl

main :: IO ()
main = print . take 866
	$ peekMultOWires (rrfAllOutputs trySingleCycleRrfWl)
		<$> iterate step trySingleCycleCctWl
