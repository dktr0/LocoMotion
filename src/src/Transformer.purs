module Transformer (
  Transformer(..),
  Modifier(..),
  ValueExpr(..),
  transformer,
  realizeTransformer
  )where

import Prelude
import Data.Map
import Data.List as List
import Parsing.Combinators hiding (empty)
import Data.Number
import Data.Semigroup
import Data.Tuple
import Data.Semigroup.Foldable (fold1)
import Data.Int (toNumber)
import Data.Foldable (foldl)

import TokenParser
import Value
import ValueMap

type Transformer = List.List Modifier

type Modifier = Tuple String ValueExpr

data ValueExpr =
  LiteralNumber Number |
  LiteralString String |
  LiteralInt Int |
  LiteralBoolean Boolean |
  ThisRef String | -- eg. this.x would be ValueExprThis "x"
  GlobalRef String | -- eg. x would be ValueExprGlobal "x"
  Sum ValueExpr ValueExpr |
  Difference ValueExpr ValueExpr |
  Product ValueExpr ValueExpr |
  Divide ValueExpr ValueExpr |
  Osc ValueExpr |
  Range ValueExpr ValueExpr ValueExpr

instance Show ValueExpr where
  show (LiteralNumber x) = "LiteralNumber " <> show x
  show (LiteralString x) = "LiteralString " <> show x
  show (LiteralInt x) = "LiteralInt " <> show x
  show (LiteralBoolean x) = "LiteralBoolean " <> show x
  show (ThisRef x) = "this." <> x
  show (GlobalRef x) = x
  show (Sum x y) = "Sum (" <> show x <> ") (" <> show y <> ")"
  show (Difference x y) = "Difference (" <> show x <> ") (" <> show y <> ")"
  show (Product x y) = "Product (" <> show x <> ") (" <> show y <> ")"
  show (Divide x y) = "Divide (" <> show x <> ") (" <> show y <> ")"
  show (Osc x) = "Osc (" <> show x <> ")"
  show (Range r1 r2 x) = "Range (" <> show r1 <> ") (" <> show r2 <> ") (" <> show x <> ")"


realizeTransformer :: Number -> Transformer -> ValueMap
realizeTransformer nCycles t = foldl (realizeModifier nCycles) empty t

realizeModifier :: Number -> Map String Value -> Modifier -> Map String Value
realizeModifier nCycles m (Tuple k vExpr) = insert k (realizeValueExpr nCycles m vExpr) m

realizeValueExpr :: Number -> Map String Value -> ValueExpr -> Value
realizeValueExpr _ _ (LiteralNumber x) = ValueNumber x
realizeValueExpr _ _ (LiteralString x) = ValueString x
realizeValueExpr _ _ (LiteralInt x) = ValueInt x
realizeValueExpr _ _ (LiteralBoolean x) = ValueBoolean x
realizeValueExpr _ _ (ThisRef _) = ValueNumber 0.0 -- placeholder ? lookup in map but what if not found ?
realizeValueExpr _ _ (GlobalRef _) = ValueNumber 0.0 -- placeholder
realizeValueExpr nCycles m (Sum x y) = realizeValueExpr nCycles m x + realizeValueExpr nCycles m y
realizeValueExpr nCycles m (Difference x y) = realizeValueExpr nCycles m x - realizeValueExpr nCycles m y
realizeValueExpr nCycles m (Product x y) = realizeValueExpr nCycles m x * realizeValueExpr nCycles m y
realizeValueExpr nCycles m (Divide x y) = divideValues (realizeValueExpr nCycles m x) (realizeValueExpr nCycles m y)
realizeValueExpr nCycles m (Osc f) = ValueNumber $ valueToNumber (realizeValueExpr nCycles m f) * 2.0 * pi * nCycles
realizeValueExpr nCycles m (Range r1 r2 x) = ValueNumber $ (x' * 0.5 + 0.5) * (r2' - r1') + r1'
  where
    r1' = valueToNumber $ realizeValueExpr nCycles m r1
    r2' = valueToNumber $ realizeValueExpr nCycles m r2
    x' = valueToNumber $ realizeValueExpr nCycles m x


-- parsers

transformer :: P Transformer
transformer = do
  xs <- many1 transformerDictionary
  pure $ fold1 xs

transformerDictionary :: P Transformer
transformerDictionary = do
  reservedOp "{"
  fs <- commaSep modifier
  reservedOp "}"
  pure fs

modifier :: P Modifier
modifier = do
  k <- identifier
  reservedOp "="
  v <- valueExpr
  pure $ Tuple k v

valueExpr :: P ValueExpr
valueExpr = do
  _ <- pure unit
  chainl1 valueExpr' additionSubtraction

additionSubtraction :: P (ValueExpr -> ValueExpr -> ValueExpr)
additionSubtraction = choice [
  reservedOp "+" $> Sum,
  reservedOp "-" $> Difference
  ]

valueExpr' :: P ValueExpr
valueExpr' = do
  _ <- pure unit
  chainl1 valueExpr'' multiplicationDivision

multiplicationDivision :: P (ValueExpr -> ValueExpr -> ValueExpr)
multiplicationDivision = choice [
  reservedOp "*" $> Product,
  reservedOp "/" $> Divide
  ]

valueExpr'' :: P ValueExpr
valueExpr'' = do
  _ <- pure unit
  choice [
    parens valueExpr,
    try $ LiteralNumber <$> number,
    try $ LiteralString <$> stringLiteral,
    try $ LiteralInt <$> integer,
    try osc,
    try range,
    try thisRef,
    try globalRef
    ]

osc :: P ValueExpr
osc = do
  reserved "osc"
  f <- valueExprAsArgument
  pure $ Osc f

range :: P ValueExpr
range = do
  reserved "range"
  r1 <- valueExprAsArgument
  r2 <- valueExprAsArgument
  x <- valueExprAsArgument
  pure $ Range r1 r2 x

thisRef :: P ValueExpr
thisRef = do
  reserved "this"
  reservedOp "."
  ThisRef <$> identifier

globalRef :: P ValueExpr
globalRef = GlobalRef <$> identifier

valueExprAsArgument :: P ValueExpr
valueExprAsArgument = do
  _ <- pure unit
  choice [
    parens valueExpr,
    try $ LiteralNumber <$> number
    ]
