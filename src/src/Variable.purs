module Variable where

import Prelude
import Data.Tempo
import Data.Rational
import Data.Number

-- for now, a Variable is something that, at runtime
-- can be sampled on a per-animation-frame basis to
-- produce a numeric value (a Number).

data Variable =
  Constant Number |
  Sum Variable Variable |
  Product Variable Variable |
  Osc Variable

instance Show Variable where
  show (Constant x) = "Constant " <> show x
  show (Sum x y) = "Sum (" <> show x <> ") (" <> show y <> ")"
  show (Product x y) = "Product (" <> show x <> ") (" <> show y <> ")"
  show (Osc x) = "Osc (" <> show x <> ")"

-- for now, the only information provided on a per-frame
-- basis is the position in cycles in a current metric grid.
-- (so the oscillations of Osc are relative to tempo rather than in Hz.)

sampleVariable :: Rational -> Variable -> Number
sampleVariable _ (Constant x) = x
sampleVariable nCycles (Sum x y) = sampleVariable nCycles x + sampleVariable nCycles y
sampleVariable nCycles (Product x y) = sampleVariable nCycles x * sampleVariable nCycles y
sampleVariable nCycles (Osc f) = sin $ sampleVariable nCycles f * 2.0 * pi * toNumber nCycles
