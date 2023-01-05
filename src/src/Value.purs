module Value where

-- A Value represents anything that can be on the right-hand side of some assignment
-- statement (but not all Values can be on the right-hand side of particular assignments).
-- At runtime, a Value has one of a number of specific types - and it is possible to
-- implicitly cast it to any of the other types in particular circumstances where the
-- Value is consumed.

import Prelude (identity, ($), show, (/=), class Semiring, class Ring, (/), (+), (-), (*), pure, (<>), bind, (>>=), map)
import Data.Int (toNumber,floor)
import Data.Map (Map, lookup, insert, fromFoldable)
import Data.Maybe (maybe,Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Either (Either)
import Data.List (List)
import Parsing (ParseError(..),Position)
import Control.Monad.Error.Class (throwError)
import Data.Foldable (foldl)
import Data.Number (sin,pi)

import AST (Expression)
import AST as AST
import Variable

type Transformer = ValueMap -> Either ParseError ValueMap

data Value =
  ValueNumber Number | -- x = 4.0;
  ValueString String | -- x = "a string";
  ValueInt Int | -- x = 4;
  ValueBoolean Boolean | -- x = true;
  ValueVariable Variable | -- x = osc 1.0;
  ValueTransformer Transformer | -- x = { ry = this.ry + osc 1.0 };
  ValueFunction (Position -> Value -> Either ParseError Value) |
  ValueDancer Int Transformer |
  ValueFloor Int Transformer |
  ValueCamera Transformer

valueToNumber :: Value -> Number
valueToNumber (ValueNumber x) = x
valueToNumber (ValueInt x) = toNumber x
valueToNumber (ValueBoolean true) = 1.0
valueToNumber (ValueBoolean false) = 0.0
valueToNumber _ = 0.0

valueToString :: Value -> String
valueToString (ValueNumber x) = show x
valueToString (ValueString x) = x
valueToString (ValueInt x) = show x
valueToString (ValueBoolean x) = show x
valueToString _ = ""

valueToInt :: Value -> Int
valueToInt (ValueNumber x) = floor x
valueToInt (ValueInt x) = x
valueToInt (ValueBoolean true) = 1
valueToInt (ValueBoolean false) = 0
valueToInt _ = 0

valueToBoolean :: Value -> Boolean
valueToBoolean (ValueNumber x) = x /= 0.0
valueToBoolean (ValueString "true") = true
valueToBoolean (ValueInt x) = x /= 0
valueToBoolean (ValueBoolean x) = x
valueToBoolean _ = false

valueToVariable :: Value -> Variable
valueToVariable (ValueNumber x) = constantVariable x
valueToVariable (ValueInt x) = constantVariable $ toNumber x
valueToVariable (ValueBoolean true) = constantVariable 1.0
valueToVariable (ValueVariable x) = x
valueToVariable _ = constantVariable 0.0

valueToTransformer :: Value -> Transformer
valueToTransformer (ValueTransformer x) = x
valueToTransformer (ValueDancer _ x) = x
valueToTransformer (ValueFloor _ x) = x
valueToTransformer _ = pure

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
    f a b = a/b


-- ValueMap

type ValueMap = Map String Value

lookupNumber :: Number -> String -> ValueMap -> Number
lookupNumber d k m = valueToNumber $ lookupValue (ValueNumber d) k m

lookupString :: String -> String -> ValueMap -> String
lookupString d k m = valueToString $ lookupValue (ValueString d) k m

lookupInt :: Int -> String -> ValueMap -> Int
lookupInt d k m = valueToInt $ lookupValue (ValueInt d) k m

lookupBoolean :: Boolean -> String -> ValueMap -> Boolean
lookupBoolean d k m = valueToBoolean $ lookupValue (ValueBoolean d) k m

lookupVariable :: Variable -> String -> ValueMap -> Variable
lookupVariable d k m = valueToVariable $ lookupValue (ValueVariable d) k m

lookupValue :: Value -> String -> ValueMap -> Value
lookupValue d k m = maybe d identity $ lookup k m
