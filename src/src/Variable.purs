module Variable where

-- A Variable is a function from time to something that can be used to set a ThreeJS parameter
-- for now, such things are just numbers (or things that can be adequately represented with numbers)
-- It is represented as a tree/graph, rather than directly as a function, as that allows meaningful
-- Eq and Show instances.

import Prelude
import Data.Number (sin,pi,floor)

data Variable =
  ConstantVariable Number |
  Osc Variable |
  Phase Variable Variable |
  Sum Variable Variable |
  Sub Variable Variable |
  Product Variable Variable |
  Divide Variable Variable

realizeVariable :: Number -> Variable -> Number
realizeVariable _ (ConstantVariable x) = x
realizeVariable nCycles (Osc x) = osc nCycles (realizeVariable nCycles x)
realizeVariable nCycles (Phase dur offset) = phase nCycles (realizeVariable nCycles dur) (realizeVariable nCycles offset)
realizeVariable nCycles (Sum x y) = realizeVariable nCycles x + realizeVariable nCycles y
realizeVariable nCycles (Sub x y) = realizeVariable nCycles x - realizeVariable nCycles y
realizeVariable nCycles (Product x y) = realizeVariable nCycles x * realizeVariable nCycles y
realizeVariable nCycles (Divide x y) = safeDivide (realizeVariable nCycles x) (realizeVariable nCycles y)

instance Eq Variable where
  eq (ConstantVariable x) (ConstantVariable y) = x == y
  eq (Osc x) (Osc y) = x == y
  eq (Sum a b) (Sum c d) = a == c && b == d
  eq (Sub a b) (Sub c d) = a == c && b == d
  eq (Product a b) (Product c d) = a == c && b == d
  eq (Divide a b) (Divide c d) = a == c && b == d
  eq _ _ = false

instance Show Variable where
  show (ConstantVariable x) = "ConstantVariable " <> show x
  show (Osc x) = "Osc (" <> show x <> ")"
  show (Phase dur offset) = "Phase (" <> show dur <> ") (" <> show offset <> ")"
  show (Sum x y) = "Sum (" <> show x <> ") (" <> show y <> ")"
  show (Sub x y) = "Sub (" <> show x <> ") (" <> show y <> ")"
  show (Product x y) = "Product (" <> show x <> ") (" <> show y <> ")"
  show (Divide x y) = "Divide (" <> show x <> ") (" <> show y <> ")"

instance Semiring Variable where
  zero = ConstantVariable 0.0
  one = ConstantVariable 1.0
  add x y = Sum x y
  mul x y = Product x y

instance Ring Variable where
  sub x y = Sub x y

safeDivide :: Number -> Number -> Number
safeDivide _ 0.0 = 0.0
safeDivide x y = x/y

osc :: Number -> Number -> Number
osc nCycles f = sin $ f * nCycles * 2.0 * pi

phase :: Number -> Number -> Number -> Number
phase nCycles dur offset = x - floor x
  where x = safeDivide (nCycles + offset) dur



