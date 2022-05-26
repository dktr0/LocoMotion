module Variable where

import Prelude
import Math

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

sampleVariable :: Number -> Variable -> Number
sampleVariable _ (Constant x) = x
sampleVariable t (Sum x y) = sampleVariable t x + sampleVariable t y
sampleVariable t (Product x y) = sampleVariable t x * sampleVariable t y
sampleVariable t (Osc f) = sin $ sampleVariable t f * 2.0 * pi * t
