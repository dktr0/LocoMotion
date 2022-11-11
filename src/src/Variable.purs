module Variable where

-- A Variable is a function from time to something that can be used to set a ThreeJS parameter
-- for now, such things are just numbers (or things that can be adequately represented with numbers)

import Prelude

newtype Variable = Variable (Number -> Number)

realizeVariable :: Number -> Variable -> Number
realizeVariable nCycles (Variable f) = f nCycles

constantVariable :: Number -> Variable
constantVariable x = Variable $ \_ -> x

instance Semiring Variable where
  add (Variable fx) (Variable fy) = Variable $ \nCycles -> fx nCycles + fy nCycles
  zero = Variable $ \_ -> 0.0
  mul (Variable fx) (Variable fy) = Variable $ \nCycles -> fx nCycles * fy nCycles
  one = Variable $ \_ -> 1.0

instance Ring Variable where
  sub (Variable fx) (Variable fy) = Variable $ \nCycles -> fx nCycles - fy nCycles

divide :: Variable -> Variable -> Variable
divide (Variable fx) (Variable fy) = Variable $ \nCycles -> f (fx nCycles) (fy nCycles)
  where
    f _ 0.0 = 0.0
    f x y = x/y
