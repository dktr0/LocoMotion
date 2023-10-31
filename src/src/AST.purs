module AST (
  AST,
  Statement(..),
  Expression(..),
  emptyAST,
  parseAST,
  expressionPosition
  ) where

import Prelude
import Data.List (List(..),(:))
import Data.Tuple (Tuple(..))
import Parsing (Position(..),position, ParseError,runParser)
import Parsing.Combinators (chainl1, choice, lookAhead, try, (<|>), many, option)
import Parsing.String (eof)
import Data.Foldable (foldl)
import Data.Either (Either(..))


import TokenParser (P, boolean, commaSep, identifier, integer, number, parens, reserved, reservedOp, semiSep, stringLiteral, whiteSpace, naturalOrFloat, brackets)
import ElementType

type AST = List Statement

data Statement =
  Assignment Position String Expression |
  Action Expression | -- Action doesn't need to record Position since the contained Expression will have the Position
  EmptyStatement Position

instance Eq Statement where
  eq (Assignment p1 s1 e1) (Assignment p2 s2 e2) = p1 == p2 && s1 == s2 && e1 == e2
  eq (Action e1) (Action e2) = e1 == e2
  eq (EmptyStatement p1) (EmptyStatement p2) = p1 == p2
  eq _ _ = false

instance Show Statement where
  show (Assignment p k e) = "Assignment (" <> show p <> ") " <> show k <> " (" <> show e <> ")"
  show (Action e) = "Action (" <> show e <> ")"
  show (EmptyStatement p) = "EmptyStatement (" <> show p <> ")"

emptyAST :: AST
emptyAST = EmptyStatement (Position { column: 1, index: 0, line: 1 }) : Nil

parseAST :: String -> Either ParseError AST
parseAST x = runParser x ast

data Expression =
  Element Position ElementType |
  Camera Position | Osc Position | Range Position | Clear Position | Phase Position | Step Position |
  For Position | Map Position |
  LiteralNumber Position Number |
  LiteralString Position String |
  LiteralInt Position Int |
  LiteralBoolean Position Boolean |
  ListExpression Position (List Expression) |
  This Position String | -- eg. this.x would be This "x"
  SemiGlobal Position String | -- eg. x would be SemiGlobal "x"
  Transformer Position (List (Tuple String Expression)) |
  Application Position Expression Expression |
  Sum Position Expression Expression |
  Difference Position Expression Expression |
  Product Position Expression Expression |
  Divide Position Expression Expression |
  Lambda Position String Expression

instance Eq Expression where
  eq (Element p1 t1) (Element p2 t2) = p1 == p2 && t1 == t2
  eq (Camera p1) (Camera p2) = p1 == p2
  eq (Osc p1) (Osc p2) = p1 == p2
  eq (Range p1) (Range p2) = p1 == p2
  eq (Clear p1) (Clear p2) = p1 == p2
  eq (Phase p1) (Phase p2) = p1 == p2
  eq (Step p1) (Step p2) = p1 == p2
  eq (For p1) (For p2) = p1 == p2
  eq (Map p1) (Map p2) = p1 == p2
  eq (LiteralNumber p1 x1) (LiteralNumber p2 x2) = p1 == p2 && x1 == x2
  eq (LiteralString p1 x1) (LiteralString p2 x2) = p1 == p2 && x1 == x2
  eq (LiteralInt p1 x1) (LiteralInt p2 x2) = p1 == p2 && x1 == x2
  eq (LiteralBoolean p1 x1) (LiteralBoolean p2 x2) = p1 == p2 && x1 == x2
  eq (ListExpression p1 xs1) (ListExpression p2 xs2) = p1 == p2 && xs1 == xs2
  eq (This p1 x1) (This p2 x2) = p1 == p2 && x1 == x2
  eq (SemiGlobal p1 x1) (SemiGlobal p2 x2) = p1 == p2 && x1 == x2
  eq (Transformer p1 x1) (Transformer p2 x2) = p1 == p2 && x1 == x2
  eq (Application p1 x1a x1b) (Application p2 x2a x2b) = p1 == p2 && x1a == x2a && x1b == x2b
  eq (Sum p1 x1a x1b) (Sum p2 x2a x2b) = p1 == p2 && x1a == x2a && x1b == x2b
  eq (Difference p1 x1a x1b) (Difference p2 x2a x2b) = p1 == p2 && x1a == x2a && x1b == x2b
  eq (Product p1 x1a x1b) (Product p2 x2a x2b) = p1 == p2 && x1a == x2a && x1b == x2b
  eq (Divide p1 x1a x1b) (Divide p2 x2a x2b) = p1 == p2 && x1a == x2a && x1b == x2b
  eq (Lambda p1 xs1 e1) (Lambda p2 xs2 e2) = p1 == p2 && xs1 == xs2 && e1 == e2
  eq _ _ = false

instance Show Expression where
  show (LiteralNumber p x) = "LiteralNumber (" <> show p <> ") " <> show x
  show (LiteralString p x) = "(LiteralString (" <> show p <> ") " <> show x <> ")"
  show (LiteralInt p x) = "LiteralInt (" <> show p <> ") " <> show x
  show (LiteralBoolean p x) = "LiteralBoolean (" <> show p <> ") " <> show x
  show (ListExpression p xs) = "ListExpression (" <> show p <> ") (" <> show xs <> ")"
  show (This p x) = "This (" <> show p <> ") " <> show x
  show (SemiGlobal p x) = "SemiGlobal (" <> show p <> ") " <> show x
  show (Application p e1 e2) = "Application (" <> show p <> ") (" <> show e1 <> ") (" <> show e2 <> ")"
  show (Transformer p x) = "Transformer (" <> show p <> ") (" <> show x <> ")"
  show (Element p t) = "(Element " <> show t <> "(" <> show p <> "))"
  show (Camera p) = "Camera (" <> show p <> ")"
  show (Osc p) = "Osc (" <> show p <> ")"
  show (Range p) = "Range (" <> show p <> ")"
  show (Clear p) = "Clear (" <> show p <> ")"
  show (Phase p) = "Phase (" <> show p <> ")"
  show (Step p) = "Step (" <> show p <> ")"
  show (For p) = "For (" <> show p <> ")"
  show (Map p) = "Map (" <> show p <> ")"
  show (Sum p e1 e2) = "Sum (" <> show p <> ") (" <> show e1 <> ") (" <> show e2 <> ")"
  show (Difference p e1 e2) = "Difference (" <> show p <> ") (" <> show e1 <> ") (" <> show e2 <> ")"
  show (Product p e1 e2) = "Product (" <> show p <> ") (" <> show e1 <> ") (" <> show e2 <> ")"
  show (Divide p e1 e2) = "Divide (" <> show p <> ") (" <> show e1 <> ") (" <> show e2 <> ")"
  show (Lambda p xs e) = "Lambda (" <> show xs <> ") (" <> show e <> ")"

expressionPosition :: Expression -> Position
expressionPosition (LiteralNumber p _) = p
expressionPosition (LiteralString p _) = p
expressionPosition (LiteralInt p _) = p
expressionPosition (LiteralBoolean p _) = p
expressionPosition (ListExpression p _) = p
expressionPosition (This p _) = p
expressionPosition (SemiGlobal p _) = p
expressionPosition (Application p _ _) = p
expressionPosition (Transformer p _) = p
expressionPosition (Element p _) = p
expressionPosition (Camera p) = p
expressionPosition (Osc p) = p
expressionPosition (Range p) = p
expressionPosition (Clear p) = p
expressionPosition (Phase p) = p
expressionPosition (Step p) = p
expressionPosition (For p) = p
expressionPosition (Map p) = p
expressionPosition (Sum p _ _) = p
expressionPosition (Difference p _ _) = p
expressionPosition (Product p _ _) = p
expressionPosition (Divide p _ _) = p
expressionPosition (Lambda p _ _) = p


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
  p <- position
  k <- identifier
  reservedOp "="
  v <- expression
  pure $ Assignment p k v

action :: P Statement
action = Action <$> expression

emptyStatement :: P Statement
emptyStatement = do
  p <- position
  lookAhead whiteSpace
  lookAhead eof <|> lookAhead (reservedOp ";")
  pure $ EmptyStatement p

expression :: P Expression
expression = do
  _ <- pure unit
  chainl1 expression' additionSubtraction

additionSubtraction :: P (Expression -> Expression -> Expression)
additionSubtraction = do
  p <- position
  choice [
    reservedOp "+" $> Sum p,
    reservedOp "-" $> Difference p
    ]

expression' :: P Expression
expression' = do
  _ <- pure unit
  chainl1 expression'' multiplicationDivision

multiplicationDivision :: P (Expression -> Expression -> Expression)
multiplicationDivision = do
  p <- position
  choice [
    reservedOp "*" $> Product p,
    reservedOp "/" $> Divide p
    ]

expression'' :: P Expression
expression'' = do
  _ <- pure unit
  choice [
    try application,
    argument
    ]

application :: P Expression
application = do
  _ <- pure unit
  p <- position
  f <- argument
  firstArg <- argument
  otherArgs <- many argument
  pure $ foldl (Application p) (Application p f firstArg) otherArgs

argument :: P Expression
argument = do
  _ <- pure unit
  p <- position
  choice [
    parens expression,
    try list,
    try transformer,
    try intOrNumber,
    try $ LiteralString p <$> stringLiteral,
    try $ LiteralBoolean p <$> boolean,
    try (Element p Dancer <$ reserved "dancer"),
    try (Element p Plane <$ reserved "plane"),
    try (Element p Ambient <$ reserved "ambient"),
    try (Element p Directional <$ reserved "directional"),
    try (Element p Hemisphere <$ reserved "hemisphere"),
    try (Element p Point <$ reserved "point"),
    try (Element p RectArea <$ reserved "rectarea"),
    try (Element p Spot <$ reserved "spot"),
    try (Camera p <$ reserved "camera"),
    try (Osc p <$ reserved "osc"),
    try (Range p <$ reserved "range"),
    try (Clear p <$ reserved "clear"),
    try (Phase p <$ reserved "phase"),
    try (Step p <$ reserved "step"),
    try (For p <$ reserved "for"),
    try (Map p <$ reserved "map"),
    try lambda,
    try thisRef,
    semiGlobalRef
  ]

list :: P Expression
list = do
  p <- position
  xs <- brackets $ commaSep expression
  pure $ ListExpression p xs

intOrNumber :: P Expression
intOrNumber = do
  p <- position
  isPositive <- option true (false <$ reservedOp "-")
  x <- naturalOrFloat
  case x of
    Left i -> if isPositive then (pure $ LiteralInt p i) else (pure $ LiteralInt p (i*(-1)))
    Right f -> if isPositive then (pure $ LiteralNumber p f) else (pure $ LiteralNumber p (f*(-1.0)))

transformer :: P Expression
transformer = do
  _ <- pure unit
  p <- position
  reservedOp "{"
  xs <- commaSep modifier
  reservedOp "}"
  pure $ Transformer p xs

modifier :: P (Tuple String Expression)
modifier = do
  k <- identifier
  (reservedOp "=" <|> reservedOp ":")
  e <- expression
  pure $ Tuple (translateModifierIdentifiers k) e

translateModifierIdentifiers :: String -> String
translateModifierIdentifiers "color" = "colour"
translateModifierIdentifiers x = x

thisRef :: P Expression
thisRef = do
  p <- position
  reserved "this"
  reservedOp "."
  This p <$> identifier

semiGlobalRef :: P Expression
semiGlobalRef = SemiGlobal <$> position <*> identifier


lambda :: P Expression
lambda = do
  p <- position
  reservedOp "\\"
  x <- identifier
  reservedOp "->"
  e <- expression
  pure $ Lambda p x e
