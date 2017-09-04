{-# LANGUAGE DataKinds     #-}

module NoCPR where

import           CLaSH.Prelude

{-# ANN example
  (defTop { t_name = "example"
          , t_inputs = [ PortName "a"
                       ]
          , t_output = PortField ""
                         [ PortName "b"
                         , PortName "c"
                         ]
          }
  )#-}

example :: Signal System (BitVector 1) -> Signal System (BitVector 1, BitVector 1)
example input = foo $ bundle (input, pure 0)

{-# NOINLINE foo #-}
foo :: Signal domain (BitVector 1, BitVector 1)
    -> Signal domain (BitVector 1, BitVector 1)
foo input = input
