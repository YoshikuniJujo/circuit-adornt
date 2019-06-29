{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import Circuit
import Memory
import TryInstControlMp

main :: IO ()
main = print . take 2000
	$ peekMultOWires (rrfAllOutputs trySingleCycleRrfMp)
		<$> iterate step trySingleCycleCctMp
