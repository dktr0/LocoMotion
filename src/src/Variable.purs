module Variable (
  Variable(..),
  sampleVariable
  ) where

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
  Difference Variable Variable |
  Product Variable Variable |
  Divide Variable Variable |
  Osc Variable |
  Range Variable Variable Variable

instance Show Variable where
  show (Constant x) = "Constant " <> show x
  show (Sum x y) = "Sum (" <> show x <> ") (" <> show y <> ")"
  show (Difference x y) = "Difference (" <> show x <> ") (" <> show y <> ")"
  show (Product x y) = "Product (" <> show x <> ") (" <> show y <> ")"
  show (Divide x y) = "Divide (" <> show x <> ") (" <> show y <> ")"
  show (Osc x) = "Osc (" <> show x <> ")"
  show (Range r1 r2 x) = "Range (" <> show r1 <> ") (" <> show r2 <> ") (" <> show x <> ")"

-- for now, the only information provided on a per-frame
-- basis is the position in cycles in a current metric grid.
-- (so the oscillations of Osc are relative to tempo rather than in Hz.)

sampleVariable :: Number -> Variable -> Number
sampleVariable _ (Constant x) = x
sampleVariable nCycles (Sum x y) = sampleVariable nCycles x + sampleVariable nCycles y
sampleVariable nCycles (Difference x y) = sampleVariable nCycles x - sampleVariable nCycles y
sampleVariable nCycles (Product x y) = sampleVariable nCycles x * sampleVariable nCycles y
sampleVariable nCycles (Divide x y) = safeDivide (sampleVariable nCycles x) (sampleVariable nCycles y)
sampleVariable nCycles (Osc f) = sin $ sampleVariable nCycles f * 2.0 * pi * nCycles
sampleVariable nCycles (Range r1 r2 x) = rangeVariable (sampleVariable nCycles r1) (sampleVariable nCycles r2) (sampleVariable nCycles x)

safeDivide :: Number -> Number -> Number
safeDivide _ 0.0 = 0.0
safeDivide x y = x/y

rangeVariable :: Number -> Number -> Number -> Number
rangeVariable r1 r2 x = (x * 0.5 + 0.5) * (r2 - r1) + r1
