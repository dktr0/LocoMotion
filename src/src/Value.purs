module Value where

import Prelude (identity, ($))
import Data.Int (toNumber,floor)
import Data.Map (Map, lookup)
import Data.Maybe (maybe)

import AST (Expression)
import AST as AST
import Variable

data Value =
  ValueNumber Number |
  ValueString String |
  ValueInt Int |
  ValueBoolean Boolean |
  ValueVariable Variable

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
valueToVariable (ValueBoolean false) = constantVariable 0.0
valueToVariable (ValueVariable x) = x
valueToVariable _ =

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


-- parsing from AST to Values

expressionToValue :: ValueMap -> ValueMap -> Expression -> Either ParseError Value
expressionToValue _ _ (AST.LiteralNumber x) = pure $ ValueNumber x
expressionToValue _ _ (AST.LiteralString x) = pure $ ValueString x
expressionToValue _ _ (AST.LiteralInt x) = pure $ ValueInt x
expressionToValue _ _ (AST.LiteralBoolean x) = pure $ ValueBoolean x
expressionToValue _ thisMap (AST.This k) = do
  case lookup k thisMap of
    Nothing -> ...error, no definition named this.k exists...
    Just v -> pure v
expressionToValue semiMap _ (AST.SemiGlobal k) = do
  case lookup k semiMap of
    Nothing -> ...error, no definition named k exists...
    Just v -> pure v
expressionToValue semiMap thisMap (AST.Application e1 e2) = applicationExpressionToValue semiMap thisMap e1 e2
expressionToValue semiMap thisMap (AST.Transformer xs) = transformerExpressionToValue semiMap thisMap xs
expressionToValue _ _ AST.Dancer = ...error, since can't be a value
expressionToValue _ _ AST.Floor = ...error, since can't be a value
expressionToValue _ _ AST.Camera = ...error, since can't be a value
expressionToValue _ _ AST.Osc = ...error, since can't be a value
expressionToValue _ _ AST.Range = ...error, since can't be a value
expressionToValue semiMap thisMap (AST.Sum e1 e2) = do
  e1' <- expressionToValue semiMap thisMap e1
  e2' <- expressionToValue semiMap thisMap e2
  pure $ e1' + e2'
expressionToValue semiMap thisMap (AST.Difference e1 e2) = do
  e1' <- expressionToValue semiMap thisMap e1
  e2' <- expressionToValue semiMap thisMap e2
  pure $ e1' - e2'
expressionToValue semiMap thisMap (AST.Product e1 e2) = do
  e1' <- expressionToValue semiMap thisMap e1
  e2' <- expressionToValue semiMap thisMap e2
  pure $ e1' * e2'
expressionToValue semiMap thisMap (AST.Divide e1 e2) = do
  e1' <- expressionToValue semiMap thisMap e1
  e2' <- expressionToValue semiMap thisMap e2
  pure $ divideValues e1 e2


applicationExpressionToValue :: ValueMap -> ValueMap -> Expression -> Expression -> Either ParseError Value
applicationExpressionToValue semiMap thisMap e1 e2 = do
  ...

-- thinking here: normal function application kind of requires AST to be left associative, but we have it right associative...


transformerExpressionToValue :: ValueMap -> ValueMap -> List (Tuple String Expression) -> Either ParseError Value
transformerExpressionToValue semiMap thisMap xs = do
  ...
