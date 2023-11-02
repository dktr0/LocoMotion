module AST (
  AST,
  Statement(..),
  Expression(..),
  emptyAST,
  parseAST,
  expressionPosition
  ) where

import Prelude
import Data.List (List(..),(:),range)
import Data.Tuple (Tuple(..))
import Parsing (Position(..),position, ParseError,runParser)
import Parsing.Combinators (chainl1, choice, lookAhead, try, (<|>), many, option)
import Parsing.String (eof)
import Data.Foldable (foldl)
import Data.Either (Either(..))


import TokenParser (P, boolean, commaSep, identifier, integer, number, parens, reserved, reservedOp, semiSep, stringLiteral, whiteSpace, naturalOrFloat, brackets, comma, reservedNamesDef)
import ElementType

type AST = List Statement

data Statement =
  Assignment Position String (List String) Expression | -- position name arguments body
  Action Expression | -- Action doesn't need to record Position since the contained Expression will have the Position
  EmptyStatement Position

instance Eq Statement where
  eq (Assignment p1 n1 as1 e1) (Assignment p2 n2 as2 e2) = p1 == p2 && n1 == n2 && as1 == as2 && e1 == e2
  eq (Action e1) (Action e2) = e1 == e2
  eq (EmptyStatement p1) (EmptyStatement p2) = p1 == p2
  eq _ _ = false

instance Show Statement where
  show (Assignment p name args body) = "Assignment (" <> show p <> ") " <> show name <> " (" <> show args <> ") (" <> show body <> ")"
  show (Action e) = "Action (" <> show e <> ")"
  show (EmptyStatement p) = "EmptyStatement (" <> show p <> ")"

emptyAST :: AST
emptyAST = EmptyStatement (Position { column: 1, index: 0, line: 1 }) : Nil

parseAST :: String -> Either ParseError AST
parseAST x = runParser x ast

data Expression =
  Reserved Position String | 
  Identifier Position String |
  LiteralNumber Position Number |
  LiteralString Position String |
  LiteralInt Position Int |
  LiteralBoolean Position Boolean |
  ListExpression Position (List Expression) |
  This Position String | -- eg. this.x would be This "x"
  Transformer Position (List (Tuple String Expression)) |
  Application Position Expression Expression |
  Sum Position Expression Expression |
  Difference Position Expression Expression |
  Product Position Expression Expression |
  Divide Position Expression Expression |
  Lambda Position (List String) Expression

instance Eq Expression where
  eq (Reserved p1 n1) (Reserved p2 n2) = p1 == p2 && n1 == n2
  eq (Identifier p1 n1) (Identifier p2 n2) = p1 == p2 && n1 == n2
  eq (LiteralNumber p1 x1) (LiteralNumber p2 x2) = p1 == p2 && x1 == x2
  eq (LiteralString p1 x1) (LiteralString p2 x2) = p1 == p2 && x1 == x2
  eq (LiteralInt p1 x1) (LiteralInt p2 x2) = p1 == p2 && x1 == x2
  eq (LiteralBoolean p1 x1) (LiteralBoolean p2 x2) = p1 == p2 && x1 == x2
  eq (ListExpression p1 xs1) (ListExpression p2 xs2) = p1 == p2 && xs1 == xs2
  eq (This p1 x1) (This p2 x2) = p1 == p2 && x1 == x2
  eq (Transformer p1 x1) (Transformer p2 x2) = p1 == p2 && x1 == x2
  eq (Application p1 x1a x1b) (Application p2 x2a x2b) = p1 == p2 && x1a == x2a && x1b == x2b
  eq (Sum p1 x1a x1b) (Sum p2 x2a x2b) = p1 == p2 && x1a == x2a && x1b == x2b
  eq (Difference p1 x1a x1b) (Difference p2 x2a x2b) = p1 == p2 && x1a == x2a && x1b == x2b
  eq (Product p1 x1a x1b) (Product p2 x2a x2b) = p1 == p2 && x1a == x2a && x1b == x2b
  eq (Divide p1 x1a x1b) (Divide p2 x2a x2b) = p1 == p2 && x1a == x2a && x1b == x2b
  eq (Lambda p1 xs1 e1) (Lambda p2 xs2 e2) = p1 == p2 && xs1 == xs2 && e1 == e2
  eq _ _ = false

instance Show Expression where
  show (Reserved p x) = "Reserved (" <> show p <> ") " <> show x
  show (Identifier p x) = "Identifier (" <> show p <> ") " <> show x
  show (LiteralNumber p x) = "LiteralNumber (" <> show p <> ") " <> show x
  show (LiteralString p x) = "(LiteralString (" <> show p <> ") " <> show x <> ")"
  show (LiteralInt p x) = "LiteralInt (" <> show p <> ") " <> show x
  show (LiteralBoolean p x) = "LiteralBoolean (" <> show p <> ") " <> show x
  show (ListExpression p xs) = "ListExpression (" <> show p <> ") (" <> show xs <> ")"
  show (This p x) = "This (" <> show p <> ") " <> show x
  show (Application p e1 e2) = "Application (" <> show p <> ") (" <> show e1 <> ") (" <> show e2 <> ")"
  show (Transformer p x) = "Transformer (" <> show p <> ") (" <> show x <> ")"
  show (Sum p e1 e2) = "Sum (" <> show p <> ") (" <> show e1 <> ") (" <> show e2 <> ")"
  show (Difference p e1 e2) = "Difference (" <> show p <> ") (" <> show e1 <> ") (" <> show e2 <> ")"
  show (Product p e1 e2) = "Product (" <> show p <> ") (" <> show e1 <> ") (" <> show e2 <> ")"
  show (Divide p e1 e2) = "Divide (" <> show p <> ") (" <> show e1 <> ") (" <> show e2 <> ")"
  show (Lambda p xs e) = "Lambda (" <> show p <> ") (" <> show xs <> ") (" <> show e <> ")"

expressionPosition :: Expression -> Position
expressionPosition (Reserved p _) = p
expressionPosition (Identifier p _) = p
expressionPosition (LiteralNumber p _) = p
expressionPosition (LiteralString p _) = p
expressionPosition (LiteralInt p _) = p
expressionPosition (LiteralBoolean p _) = p
expressionPosition (ListExpression p _) = p
expressionPosition (This p _) = p
expressionPosition (Application p _ _) = p
expressionPosition (Transformer p _) = p
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
  name <- identifier
  args <- many identifier
  reservedOp "="
  body <- expression
  pure $ Assignment p name args body

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
    try reservedNames,
    try lambda,
    try thisRef,
    Identifier p <$> identifier
  ]

reservedNames :: P Expression
reservedNames = choice $ map (try <<< reservedName) reservedNamesDef
  
reservedName :: String -> P Expression
reservedName x = do
  p <- position
  Reserved p x <$ reserved x 
  
list :: P Expression
list = do
  _ <- pure unit
  brackets $ (try list2 <|> list1)  -- not super sure why list2 needs to be first (need to make some tests to protect here)
  
list1 :: P Expression
list1 = do
  p <- position
  xs <- commaSep expression
  pure $ ListExpression p xs
  
list2 :: P Expression
list2 = do
  p <- position
  x <- integer
  reservedOp ".."
  y <- integer
  pure $ ListExpression p $ map (LiteralInt p) $ range x y  
  
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

lambda :: P Expression
lambda = do
  p <- position
  reservedOp "\\"
  xs <- many identifier
  reservedOp "->"
  e <- expression
  pure $ Lambda p xs e
