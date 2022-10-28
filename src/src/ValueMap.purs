module ValueMap where

import Prelude (identity, ($))
import Data.Map (Map, lookup)
import Data.Maybe (maybe)

import Value


type ValueMap = Map String Value

lookupNumber :: Number -> String -> ValueMap -> Number
lookupNumber d k m = valueToNumber $ lookupValue (ValueNumber d) k m

lookupString :: String -> String -> ValueMap -> String
lookupString d k m = valueToString $ lookupValue (ValueString d) k m

lookupInt :: Int -> String -> ValueMap -> Int
lookupInt d k m = valueToInt $ lookupValue (ValueInt d) k m

lookupBoolean :: Boolean -> String -> ValueMap -> Boolean
lookupBoolean d k m = valueToBoolean $ lookupValue (ValueBoolean d) k m

lookupValue :: Value -> String -> ValueMap -> Value
lookupValue d k m = maybe d identity $ lookup k m
