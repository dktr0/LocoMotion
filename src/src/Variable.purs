module Variable where

-- A Variable is a function from time to something that can be used to set a ThreeJS parameter
-- for now, such things are just numbers (or things that can be adequately represented with numbers)
-- It is represented as a tree/graph, rather than directly as a function, as that allows meaningful
-- Eq and Show instances.

import Prelude
import Data.Number (sin,pi,floor,abs)
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
  Osc Variable Variable |
  Phase Variable Variable |
  Step (List Variable) Variable |
  Sum Variable Variable |
  Sub Variable Variable |
  Product Variable Variable |
  Divide Variable Variable |
  Sin Variable  |
  RGB Variable Variable Variable |
  HSV Variable Variable Variable |
  Rnd Variable


realizeVariable :: RenderEnvironment -> Variable -> Number
realizeVariable _ (ConstantVariable x) = x
realizeVariable re CPS = Rational.toNumber re.tempo.freq
realizeVariable re Cycle = re.cycle
realizeVariable re Time = re.time
realizeVariable re Beat = re.beat
realizeVariable re (Osc f phs) = osc re.beat (realizeVariable re f) (realizeVariable re phs)
realizeVariable re (Phase dur offset) = phase re.beat (realizeVariable re dur) (realizeVariable re offset)
realizeVariable re (Step xs phs) = step (map (realizeVariable re) xs) (realizeVariable re phs)
realizeVariable re (Sum x y) = realizeVariable re x + realizeVariable re y
realizeVariable re (Sub x y) = realizeVariable re x - realizeVariable re y
realizeVariable re (Product x y) = realizeVariable re x * realizeVariable re y
realizeVariable re (Divide x y) = safeDivide (realizeVariable re x) (realizeVariable re y)
realizeVariable re (Sin x) = sin $ realizeVariable re x
realizeVariable re (RGB r g b) = rgb (realizeVariable re r) (realizeVariable re g) (realizeVariable re b)
realizeVariable re (HSV h s v) = hsv (realizeVariable re h) (realizeVariable re s) (realizeVariable re v)
realizeVariable re (Rnd x) = rnd re (realizeVariable re x)

instance Eq Variable where
  eq (ConstantVariable x) (ConstantVariable y) = x == y
  eq CPS CPS = true
  eq Cycle Cycle = true
  eq Time Time = true
  eq Beat Beat = true
  eq (Osc f1 phs1) (Osc f2 phs2) = f1 == f2 && phs1 == phs2
  eq (Phase x1 y1) (Phase x2 y2) = x1 == x2 && y1 == y2
  eq (Step xs1 phs1) (Step xs2 phs2) = xs1 == xs2 && phs1 == phs2
  eq (Sum a b) (Sum c d) = a == c && b == d
  eq (Sub a b) (Sub c d) = a == c && b == d
  eq (Product a b) (Product c d) = a == c && b == d
  eq (Divide a b) (Divide c d) = a == c && b == d
  eq (Sin x) (Sin y) = x == y
  eq (RGB r1 g1 b1) (RGB r2 g2 b2) = r1 == r2 && g1 == g2 && b1 == b2
  eq (HSV h1 s1 v1) (HSV h2 s2 v2) = h1 == h2 && s1 == s2 && v1 == v2
  eq (Rnd x) (Rnd y) = x == y
  eq _ _ = false

instance Show Variable where
  show (ConstantVariable x) = "ConstantVariable " <> show x
  show CPS = "CPS"
  show Cycle = "Cycle"
  show Time = "Time"
  show Beat = "Beat"
  show (Osc f phs) = "Osc (" <> show f <> ") (" <> show phs <> ")"
  show (Phase dur offset) = "Phase (" <> show dur <> ") (" <> show offset <> ")"
  show (Step xs phs) = "Step (" <> show xs <> ") (" <> show phs <> ")"
  show (Sum x y) = "Sum (" <> show x <> ") (" <> show y <> ")"
  show (Sub x y) = "Sub (" <> show x <> ") (" <> show y <> ")"
  show (Product x y) = "Product (" <> show x <> ") (" <> show y <> ")"
  show (Divide x y) = "Divide (" <> show x <> ") (" <> show y <> ")"
  show (Sin x) = "Sin (" <> show x <> ")"
  show (RGB r g b) = "RGB (" <> show r <> ") (" <> show g <> ") (" <> show b <> ")"
  show (HSV h s v) = "RGB (" <> show h <> ") (" <> show s <> ") (" <> show v <> ")"
  show (Rnd x) = "Rnd (" <> show x <> ")"

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

osc :: Number -> Number -> Number -> Number
osc nCycles f phs = sin $ (f * nCycles * 2.0 * pi) + (2.0 * pi * phs)

phase :: Number -> Number -> Number -> Number
phase nCycles dur offset = x - floor x
  where x = safeDivide (nCycles - offset) dur

step :: List Number -> Number -> Number
step xs phs
  | length xs <= 0 = 0.0
  | otherwise = fromMaybe 0.0 $ index xs $ Int.floor $ (phs - floor phs) * (Int.toNumber (length xs))

rgb :: Number -> Number -> Number -> Number
rgb r g b = (r*255.0*256.0*256.0) + (g*255.0*256.0) + (b*255.0)

hsv :: Number -> Number -> Number -> Number
hsv h s v = v * rgb r'' g'' b''
  where
    k1 = 1.0
    k2 = 2.0/3.0
    k3 = 1.0/3.0
    k4 = 3.0
    r = abs $ (fract $ h + 1.0) * 6.0 - 3.0
    g = abs $ (fract $ h + 2.0/3.0) * 6.0 - 3.0
    b = abs $ (fract $ h + 1.0/3.0) * 6.0 - 3.0
    r' = clamp 0.0 1.0 $ abs r - 1.0
    g' = clamp 0.0 1.0 $ abs g - 1.0
    b' = clamp 0.0 1.0 $ abs b - 1.0
    r'' = mix 1.0 r' s
    g'' = mix 1.0 g' s
    b'' = mix 1.0 b' s

fract :: Number -> Number
fract x = x - floor x

mix :: Number -> Number -> Number -> Number
mix r1 r2 x = x * (r2 - r1) + r1

rnd :: RenderEnvironment -> Number -> Number
rnd _ x = x * 123456789.0 -- placeholder
