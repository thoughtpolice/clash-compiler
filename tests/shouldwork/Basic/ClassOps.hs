module ClassOps where

import CLaSH.Prelude

topEntity :: (Integer,Integer) -> Integer
topEntity = uncurry mod
{-# NOINLINE topEntity #-}

testBench :: Signal System Bool
testBench =
  let ?clk = clockGen      @System
      ?res = asyncResetGen @System
  in  hdlSimFinish (expectedOutput (topEntity <$> testInput))
  where
    testInput      = stimuliGenerator $(v [(19,4)::(Integer,Integer),(7,3),(55,-10),(9,-2),(0,-10),(11,10)])
    expectedOutput = outputVerifier   $(v ([3::Integer,1,-5,-1,0,1]))
