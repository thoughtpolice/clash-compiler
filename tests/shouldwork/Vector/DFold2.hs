{-# LANGUAGE RankNTypes #-}
module DFold2 where

import CLaSH.Prelude
import Data.Proxy

topEntity :: Vec 4 (Vec 4 (Unsigned 8)) -> Vec 4 (Vec 4 (Unsigned 8))
topEntity = smap (flip rotateRightS)

testInput :: Signal (Vec 4 (Vec 4 (Unsigned 8)))
testInput = pure (replicate d4 (0 :> 1 :> 2 :> 3 :> Nil))

expectedOutput :: Signal (Vec 4 (Vec 4 (Unsigned 8)))
               -> Signal Bool
expectedOutput = outputVerifier (((0:>1:>2:>3:>Nil):>
                                  (3:>0:>1:>2:>Nil):>
                                  (2:>3:>0:>1:>Nil):>
                                  (1:>2:>3:>0:>Nil):>Nil):>Nil)