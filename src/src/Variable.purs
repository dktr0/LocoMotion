module Variable where

-- A Variable is a function from time to something that can be used to set a ThreeJS parameter
-- for now, such things are just numbers (or things that can be adequately represented with numbers)
-- It is represented as a tree/graph, rather than directly as a function, as that allows meaningful
-- Eq and Show instances.

import Prelude
import Data.Number (sin,pi,floor)
import Data.List (List,length,index)
import Data.Maybe (fromMaybe)
import Data.Int as Int
import Data.Rational as Rational

import RenderEnvironment

data Variable =
  ConstantVariable Number |
  CPS |
  Cycle | 
  Time |
  Beat |
  Osc Variable |
  Phase Variable Variable |
  Step (List Variable) Variable |
  Sum Variable Variable |
  Sub Variable Variable |
  Product Variable Variable |
  Divide Variable Variable |
  Sin Variable

realizeVariable :: RenderEnvironment -> Variable -> Number
realizeVariable _ (ConstantVariable x) = x
realizeVariable re CPS = Rational.toNumber re.tempo.freq
realizeVariable re Cycle = re.cycle
realizeVariable re Time = re.time
realizeVariable re Beat = re.beat
realizeVariable re (Osc x) = osc re.beat (realizeVariable re x)
realizeVariable re (Phase dur offset) = phase re.beat (realizeVariable re dur) (realizeVariable re offset)
realizeVariable re (Step xs phs) = step (map (realizeVariable re) xs) (realizeVariable re phs)
realizeVariable re (Sum x y) = realizeVariable re x + realizeVariable re y
realizeVariable re (Sub x y) = realizeVariable re x - realizeVariable re y
realizeVariable re (Product x y) = realizeVariable re x * realizeVariable re y
realizeVariable re (Divide x y) = safeDivide (realizeVariable re x) (realizeVariable re y)
realizeVariable re (Sin x) = sin $ realizeVariable re x

instance Eq Variable where
  eq (ConstantVariable x) (ConstantVariable y) = x == y
  eq CPS CPS = true
  eq Cycle Cycle = true
  eq Time Time = true
  eq Beat Beat = true
  eq (Osc x) (Osc y) = x == y
  eq (Phase x1 y1) (Phase x2 y2) = x1 == x2 && y1 == y2
  eq (Step xs1 phs1) (Step xs2 phs2) = xs1 == xs2 && phs1 == phs2
  eq (Sum a b) (Sum c d) = a == c && b == d
  eq (Sub a b) (Sub c d) = a == c && b == d
  eq (Product a b) (Product c d) = a == c && b == d
  eq (Divide a b) (Divide c d) = a == c && b == d
  eq (Sin x) (Sin y) = x == y
  eq _ _ = false

instance Show Variable where
  show (ConstantVariable x) = "ConstantVariable " <> show x
  show CPS = "CPS"
  show Cycle = "Cycle"
  show Time = "Time"
  show Beat = "Beat"
  show (Osc x) = "Osc (" <> show x <> ")"
  show (Phase dur offset) = "Phase (" <> show dur <> ") (" <> show offset <> ")"
  show (Step xs phs) = "Step (" <> show xs <> ") (" <> show phs <> ")"
  show (Sum x y) = "Sum (" <> show x <> ") (" <> show y <> ")"
  show (Sub x y) = "Sub (" <> show x <> ") (" <> show y <> ")"
  show (Product x y) = "Product (" <> show x <> ") (" <> show y <> ")"
  show (Divide x y) = "Divide (" <> show x <> ") (" <> show y <> ")"
  show (Sin x) = "Sin (" <> show x <> ")"

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
  where x = safeDivide (nCycles - offset) dur

step :: List Number -> Number -> Number
step xs phs 
  | length xs <= 0 = 0.0
  | otherwise = fromMaybe 0.0 $ index xs $ Int.floor $ (phs - floor phs) * (Int.toNumber (length xs))

