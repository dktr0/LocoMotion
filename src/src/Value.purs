module Value where

import Prelude
import Data.Int (toNumber,floor)

data Value =
  ValueNumber Number |
  ValueString String |
  ValueInt Int |
  ValueBoolean Boolean

valueToNumber :: Value -> Number
valueToNumber (ValueNumber x) = x
valueToNumber (ValueString _) = 0.0
valueToNumber (ValueInt x) = toNumber x
valueToNumber (ValueBoolean true) = 1.0
valueToNumber (ValueBoolean false) = 0.0

valueToString :: Value -> String
valueToString (ValueNumber x) = show x
valueToString (ValueString x) = x
valueToString (ValueInt x) = show x
valueToString (ValueBoolean x) = show x

valueToInt :: Value -> Int
valueToInt (ValueNumber x) = floor x
valueToInt (ValueString _) = 0
valueToInt (ValueInt x) = x
valueToInt (ValueBoolean true) = 1
valueToInt (ValueBoolean false) = 0

valueToBoolean :: Value -> Boolean
valueToBoolean (ValueNumber x) = x /= 0.0
valueToBoolean (ValueString "true") = true
valueToBoolean (ValueString _) = false
valueToBoolean (ValueInt x) = x /= 0
valueToBoolean (ValueBoolean x) = x

instance Semiring Value where
  add (ValueNumber x) y = ValueNumber $ x + valueToNumber y
  add x (ValueNumber y) = ValueNumber $ valueToNumber x + y
  add x y = ValueInt $ valueToInt x + valueToInt y
  zero = ValueInt 0
  mul (ValueNumber x) y = ValueNumber $ x * valueToNumber y
  mul x (ValueNumber y) = ValueNumber $ valueToNumber x * y
  mul x y = ValueInt $ valueToInt x * valueToInt y
  one = ValueInt 1

instance Ring Value where
  sub (ValueNumber x) y = ValueNumber $ x - valueToNumber y
  sub x (ValueNumber y) = ValueNumber $ valueToNumber x - y
  sub x y = ValueInt $ valueToInt x - valueToInt y

divideValues :: Value -> Value -> Value
divideValues x y = ValueNumber $ f (valueToNumber x) (valueToNumber y)
  where
    f _ 0.0 = 0.0
    f x y = x/y
