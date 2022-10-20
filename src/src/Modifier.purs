module Modifier (
  Modifier(..),
  ValueExpr(..),
  numberFromValueExpr,
  modifier
  )where

import Prelude
import Data.Map
import Data.List as List
import Parsing.Combinators
import Data.Number

import TokenParser


data Modifier =
  ModifierDictionary (Map String ValueExpr) |
  ModifierList (List.List Modifier)

data ValueExpr =
  LiteralNumber Number |
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
  show (ThisRef x) = "this." <> x
  show (GlobalRef x) = x
  show (Sum x y) = "Sum (" <> show x <> ") (" <> show y <> ")"
  show (Difference x y) = "Difference (" <> show x <> ") (" <> show y <> ")"
  show (Product x y) = "Product (" <> show x <> ") (" <> show y <> ")"
  show (Divide x y) = "Divide (" <> show x <> ") (" <> show y <> ")"
  show (Osc x) = "Osc (" <> show x <> ")"
  show (Range r1 r2 x) = "Range (" <> show r1 <> ") (" <> show r2 <> ") (" <> show x <> ")"

numberFromValueExpr :: Number -> ValueExpr -> Number
numberFromValueExpr _ (LiteralNumber x) = x
numberFromValueExpr _ (ThisRef x) = 0.0 -- placeholder
numberFromValueExpr _ (GlobalRef x) = 0.0 -- placeholder
numberFromValueExpr nCycles (Sum x y) = numberFromValueExpr nCycles x + numberFromValueExpr nCycles y
numberFromValueExpr nCycles (Difference x y) = numberFromValueExpr nCycles x - numberFromValueExpr nCycles y
numberFromValueExpr nCycles (Product x y) = numberFromValueExpr nCycles x * numberFromValueExpr nCycles y
numberFromValueExpr nCycles (Divide x y) = safeDivide (numberFromValueExpr nCycles x) (numberFromValueExpr nCycles y)
numberFromValueExpr nCycles (Osc f) = sin $ numberFromValueExpr nCycles f * 2.0 * pi * nCycles
numberFromValueExpr nCycles (Range r1 r2 x) = rangeVariable (numberFromValueExpr nCycles r1) (numberFromValueExpr nCycles r2) (numberFromValueExpr nCycles x)

safeDivide :: Number -> Number -> Number
safeDivide _ 0.0 = 0.0
safeDivide x y = x/y

rangeVariable :: Number -> Number -> Number -> Number
rangeVariable r1 r2 x = (x * 0.5 + 0.5) * (r2 - r1) + r1


-- parsers

modifier :: P Modifier
modifier = modifierDictionary -- modifierLists not implemented yet

modifierDictionary :: P Modifier
modifierDictionary = do
  reservedOp "{"
  fs <- commaSep modifierProperty
  reservedOp "}"
  pure $ ModifierDictionary $ unions fs

modifierProperty :: P (Map String ValueExpr)
modifierProperty = do
  k <- identifier
  reservedOp "="
  v <- valueExpr
  pure $ singleton k v

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
