module Value where

-- A Value represents anything that can be on the right-hand side of some assignment
-- statement (but not all Values can be on the right-hand side of particular assignments).
-- At runtime, a Value has one of a number of specific types - and it is possible to
-- implicitly cast it to any of the other types in particular circumstances where the
-- Value is consumed.

import Prelude
import Data.Int (toNumber,floor)
import Data.Map (Map, lookup, insert, fromFoldable, empty, union)
import Data.Array as Array
import Data.Maybe (maybe,Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Either (Either)
import Data.List (List)
import Parsing (ParseError(..),Position)
import Control.Monad.Error.Class (throwError)
import Data.Foldable (foldl)
import Data.Number (sin,pi)
import Control.Monad.State.Trans
import Data.List (singleton)

import AST (Expression)
import AST as AST
import Variable
import ElementType


data Value =
  ValueNumber Number | -- x = 4.0;
  ValueString String | -- x = "a string";
  ValueInt Int | -- x = 4;
  ValueBoolean Boolean | -- x = true;
  ValueVariable Variable | -- x = osc 1.0;
  ValueTransformer Transformer | -- x = { ry = this.ry + osc 1.0 };
  ValueFunction (Position -> Value -> Either ParseError Value) |
  ValueElement ElementType Int ValueMap |
  ValueCamera |
  ValueClear |
  ValueList (List Value)

instance Show Value where
  show (ValueNumber x) = "(ValueNumber " <> show x <> ")"
  show (ValueString x) = "(ValueString " <> show x <> ")"
  show (ValueInt x) = "(ValueInt " <> show x <> ")"
  show (ValueBoolean x) = "(ValueBoolean " <> show x <> ")"
  show (ValueVariable x) = "(ValueVariable " <> show x <> ")"
  show (ValueTransformer _) = "ValueTransformer..."
  show (ValueFunction _) = "ValueFunction... "
  show (ValueElement t i vm) = "(ValueElement " <> show t <> " " <> show i <> " (" <> show vm <> "))"
  show ValueCamera = "ValueCamera"
  show ValueClear = "ValueClear"
  show (ValueList xs) = "ValueList " <> show xs


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
valueToVariable (ValueNumber x) = ConstantVariable x
valueToVariable (ValueInt x) = ConstantVariable $ toNumber x
valueToVariable (ValueBoolean true) = ConstantVariable 1.0
valueToVariable (ValueVariable x) = x
valueToVariable _ = ConstantVariable 0.0

valueToTransformer :: Value -> Transformer
valueToTransformer (ValueTransformer x) = x
valueToTransformer _ = pure

valueToListVariable :: Value -> List Variable
valueToListVariable (ValueList xs) = map valueToVariable xs 
valueToListVariable x = singleton $ valueToVariable x


instance Semiring Value where
  zero = ValueInt 0
  one = ValueInt 1
  add (ValueVariable x) (ValueVariable y) = ValueVariable $ x + y
  add (ValueVariable x) y = ValueVariable $ x + (ConstantVariable $ valueToNumber y)
  add x (ValueVariable y) = ValueVariable $ (ConstantVariable $ valueToNumber x) + y
  add (ValueInt x) (ValueInt y) = ValueInt $ x + y
  add x y = ValueNumber $ valueToNumber x + valueToNumber y
  mul (ValueVariable x) (ValueVariable y) = ValueVariable $ x * y
  mul (ValueVariable x) y = ValueVariable $ x * (ConstantVariable $ valueToNumber y)
  mul x (ValueVariable y) = ValueVariable $ (ConstantVariable $ valueToNumber x) * y
  mul (ValueInt x) (ValueInt y) = ValueInt $ x * y
  mul x y = ValueNumber $ valueToNumber x * valueToNumber y

instance Ring Value where
  sub (ValueVariable x) (ValueVariable y) = ValueVariable $ x - y
  sub (ValueVariable x) y = ValueVariable $ x - (ConstantVariable $ valueToNumber y)
  sub x (ValueVariable y) = ValueVariable $ (ConstantVariable $ valueToNumber x) - y
  sub (ValueInt x) (ValueInt y) = ValueInt $ x - y
  sub x y = ValueNumber $ valueToNumber x - valueToNumber y

instance Eq Value where
  eq (ValueNumber x) (ValueNumber y) = x == y
  eq (ValueString x) (ValueString y) = x == y
  eq (ValueInt x) (ValueInt y) = x == y
  eq (ValueBoolean x) (ValueBoolean y) = x == y
  eq (ValueVariable x) (ValueVariable y) = x == y
  eq (ValueElement t1 i1 x) (ValueElement t2 i2 y) = t1 == t2 && i1 == i2 && x == y
  eq ValueCamera ValueCamera = true
  eq ValueClear ValueClear = true
  eq _ _ = false

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


-- Transformers

type Transformer = ValueMap -> Either ParseError ValueMap

valueMapToTransformer :: ValueMap -> Transformer
valueMapToTransformer vmNew vmOld = pure $ union vmNew vmOld

appendTransformers :: Transformer -> Transformer -> Transformer
appendTransformers fx fy = \thisMap -> fx thisMap >>= fy
