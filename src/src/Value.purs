module Value where

-- A Value represents anything that can be on the right-hand side of some assignment
-- statement (but not all Values can be on the right-hand side of particular assignments).
-- At runtime, a Value has one of a number of specific types - and it is possible to
-- implicitly cast it to any of the other types in particular circumstances where the
-- Value is consumed.

import Prelude (identity, ($), show, (/=), class Semiring, class Ring, (/), (+), (-), (*), pure, (<>), bind, discard, (>>=), map, unit)
import Data.Int (toNumber,floor)
import Data.Map (Map, lookup, insert, fromFoldable, empty)
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


import AST (Expression)
import AST as AST
import Variable


data Value =
  ValueNumber Number | -- x = 4.0;
  ValueString String | -- x = "a string";
  ValueInt Int | -- x = 4;
  ValueBoolean Boolean | -- x = true;
  ValueVariable Variable | -- x = osc 1.0;
  ValueTransformer Transformer | -- x = { ry = this.ry + osc 1.0 };
  ValueFunction (Position -> Value -> Either ParseError Value) |
  ValueDancer Int ValueMap |
  ValueFloor Int ValueMap |
  ValueCamera

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
-- valueToTransformer (ValueDancer _ x) = x -- not sure if these two pathways are necessary
-- valueToTransformer (ValueFloor _ x) = x
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


-- Transformer

type Transformer = ValueMap -> P ValueMap

valueMapToTransformer :: ValueMap -> Transformer
valueMapToTransformer vm _ = pure vm

appendTransformers :: Transformer -> Transformer -> Transformer
appendTransformers fx fy = \thisMap -> fx thisMap >>= fy

applyTransformer :: Transformer -> ValueMap -> P Value -- where Value is always a Transformer
applyTransformer tF x = do
  vm <- tF x
  pure $ ValueTransformer $ valueMapToTransformer vm


-- the P monad

type PState = {
  semiMap :: ValueMap,
  thisMap :: ValueMap,
  instantiators :: Array ValueMap,
  cameraMap :: ValueMap
  }

type P a = StateT PState (Either ParseError) a

runP :: forall a. P a -> Either ParseError a
runP p = evalStateT p {
  semiMap: empty,
  thisMap: empty,
  instantiators: [],
  cameraMap: empty
  }

-- newInstantiator and modifyInstantiator are not meant to be used from elsewhere
-- use the interface provided by new/modify-Dancer/Floor instead

newInstantiator :: ValueMap -> P Int
newInstantiator m = do
  s <- modify $ \s -> s { instantiators = Array.snoc s.instantiators m }
  pure $ Array.length s.instantiators - 1

modifyInstantiator :: Int -> Transformer -> P ValueMap
modifyInstantiator n t = do
  s <- get
  mNew <- case Array.index s.instantiators n of
    Nothing -> pure empty -- note: this should not ever happen
    Just m -> t m
  case Array.insertAt n mNew s.instantiators of
    Nothing -> pure unit  -- note: this should also not ever happen
    Just x -> put $ s { instantiators = x }
  pure mNew

newDancer :: P Value
newDancer = do
  n <- newInstantiator defaultDancer
  pure $ ValueDancer n defaultDancer

defaultDancer :: ValueMap
defaultDancer = fromFoldable [
  Tuple "x" (ValueNumber 0.0),
  Tuple "y" (ValueNumber 0.0),
  Tuple "z" (ValueNumber 0.0),
  Tuple "rx" (ValueNumber 0.0),
  Tuple "ry" (ValueNumber 0.0),
  Tuple "rz" (ValueNumber 0.0),
  Tuple "sx" (ValueNumber 1.0),
  Tuple "sy" (ValueNumber 1.0),
  Tuple "sz" (ValueNumber 1.0),
  Tuple "size" (ValueNumber 1.0)
  ]

modifyDancer :: Int -> Transformer -> P Value
modifyDancer n ty = do
  mNew <- modifyInstantiator n ty
  pure $ ValueDancer n mNew

newFloor :: P Value
newFloor = do
  n <- newInstantiator defaultFloor
  pure $ ValueFloor n defaultFloor

defaultFloor :: ValueMap
defaultFloor = fromFoldable [
  Tuple "colour" (ValueInt 0x888888),
  Tuple "shadows" (ValueBoolean true)
  ]

modifyFloor :: Int -> Transformer -> P Value
modifyFloor n ty = do
  mNew <- modifyInstantiator n ty
  pure $ ValueFloor n mNew

modifyCamera :: Transformer -> P Value
modifyCamera t = do
  s <- get
  let cm = s.cameraMap
  cm' <- t cm
  modify_ $ \x -> x { cameraMap = cm' }
  pure ValueCamera
