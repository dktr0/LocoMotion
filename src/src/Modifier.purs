module Modifier where

import Prelude
import Data.Map
import Data.List
import Parsing.Combinators

import TokenParser

data Modifier =
  ModifierDictionary (Map String ValueExpr) |
  ModifierList (List Modifier)

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
