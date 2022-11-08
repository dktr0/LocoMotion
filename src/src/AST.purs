module AST (
  AST,
  Statement(..),
  Expression(..),
  ast
  ) where

import Prelude
import Data.List (List)
import Data.Tuple (Tuple(..))
import Parsing.Combinators
import Parsing.String (eof)

import TokenParser

type AST = List Statement

data Statement =
  Assignment String Expression |
  Action Expression |
  EmptyStatement

instance Show Statement where
  show (Assignment k e) = "Assignment " <> show k <> " (" <> show e <> ")"
  show (Action e) = "Action (" <> show e <> ")"
  show EmptyStatement = "EmptyStatement"

data Expression =
  LiteralNumber Number |
  LiteralString String |
  LiteralInt Int |
  LiteralBoolean Boolean |
  This String | -- eg. this.x would be This "x"
  SemiGlobal String | -- eg. x would be SemiGlobal "x"
  Application Expression Expression |
  Transformer (List (Tuple String Expression)) |
  Dancer | Floor | Camera | Osc | Range |
  Sum Expression Expression |
  Difference Expression Expression |
  Product Expression Expression |
  Divide Expression Expression

instance Show Expression where
  show (LiteralNumber x) = show x
  show (LiteralString x) = show x
  show (LiteralInt x) = show x
  show (LiteralBoolean x) = show x
  show (This x) = "this." <> x
  show (SemiGlobal x) = x
  show (Application e1 e2) = "Application (" <> show e1 <> ") (" <> show e2 <> ")"
  show (Transformer x) = "Transformer (" <> show x <> ")"
  show Dancer = "Dancer"
  show Floor = "Floor"
  show Camera = "Camera"
  show Osc = "Osc"
  show Range = "Range"
  show (Sum e1 e2) = "Sum (" <> show e1 <> ") (" <> show e2 <> ")"
  show (Difference e1 e2) = "Difference (" <> show e1 <> ") (" <> show e2 <> ")"
  show (Product e1 e2) = "Product (" <> show e1 <> ") (" <> show e2 <> ")"
  show (Divide e1 e2) = "Divide (" <> show e1 <> ") (" <> show e2 <> ")"


-- parsing:

ast :: P AST
ast = do
  whiteSpace
  xs <- semiSep statement
  eof
  pure $ xs

statement :: P Statement
statement = try assignment <|> try action <|> emptyStatement

assignment :: P Statement
assignment = do
  k <- identifier
  reservedOp "="
  v <- expression
  pure $ Assignment k v

action :: P Statement
action = Action <$> expression

emptyStatement :: P Statement
emptyStatement = do
  lookAhead whiteSpace
  lookAhead eof <|> lookAhead (reservedOp ";")
  pure EmptyStatement

expression :: P Expression
expression = do
  _ <- pure unit
  chainl1 expression' additionSubtraction

additionSubtraction :: P (Expression -> Expression -> Expression)
additionSubtraction = choice [
  reservedOp "+" $> Sum,
  reservedOp "-" $> Difference
  ]

expression' :: P Expression
expression' = do
  _ <- pure unit
  chainl1 expression'' multiplicationDivision

multiplicationDivision :: P (Expression -> Expression -> Expression)
multiplicationDivision = choice [
  reservedOp "*" $> Product,
  reservedOp "/" $> Divide
  ]

expression'' :: P Expression
expression'' = do
  _ <- pure unit
  choice [
    parens expression,
    try transformer,
    try application,
    try $ LiteralNumber <$> number,
    try $ LiteralString <$> stringLiteral,
    try $ LiteralInt <$> integer,
    try $ LiteralBoolean <$> boolean,
    try thisRef,
    try semiGlobalRef,
    try (Dancer <$ reserved "dancer"),
    try (Floor <$ reserved "floor"),
    try (Camera <$ reserved "camera"),
    try (Osc <$ reserved "osc"),
    try (Range <$ reserved "range")
    ]

application :: P Expression
application = do
  _ <- pure unit
  e1 <- expression
  e2 <- expressionAsArgument
  pure $ Application e1 e2

-- note: this is the same as expression'' minus the direct pathway for Application
expressionAsArgument :: P Expression
expressionAsArgument = do
  _ <- pure unit
  choice [
    parens expression,
    try transformer,
    try $ LiteralNumber <$> number,
    try $ LiteralString <$> stringLiteral,
    try $ LiteralInt <$> integer,
    try $ LiteralBoolean <$> boolean,
    try thisRef,
    try semiGlobalRef,
    try (Dancer <$ reserved "dancer"),
    try (Floor <$ reserved "floor"),
    try (Camera <$ reserved "camera"),
    try (Osc <$ reserved "osc"),
    try (Range <$ reserved "range")
  ]

transformer :: P Expression
transformer = do
  _ <- pure unit
  reservedOp "{"
  xs <- commaSep modifier
  reservedOp "}"
  pure $ Transformer xs

modifier :: P (Tuple String Expression)
modifier = do
  k <- identifier
  reservedOp "="
  e <- expression
  pure $ Tuple k e

thisRef :: P Expression
thisRef = do
  reserved "this"
  reservedOp "."
  This <$> identifier

semiGlobalRef :: P Expression
semiGlobalRef = SemiGlobal <$> identifier
