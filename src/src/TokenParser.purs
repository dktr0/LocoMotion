module TokenParser where

import Prelude
import Data.Identity
import Parsing
import Parsing.Language (emptyDef)
import Parsing.Token (GenLanguageDef(..),LanguageDef,unGenLanguageDef,TokenParser,GenTokenParser,makeTokenParser)
import Parsing.Combinators
import Data.Either
import Data.List
import Data.List.NonEmpty
import Data.Int (toNumber)

type P a = ParserT String Identity a

-- low-level tokens, numbers, etc

number :: P Number
number = choice [
  try negativeFloat,
  try float,
  toNumber <$> integer
  ]

negativeFloat :: P Number
negativeFloat = do
  reservedOp "-"
  ((*) (-1.0)) <$> float

boolean :: P Boolean
boolean = choice [
  reserved "true" $> true,
  reserved "false" $> false
  ]

reservedNamesDef :: Array String
reservedNamesDef = [
  "dancer",
  "camera",
  "plane",
  "cps",
  "cycle",
  "time",
  "beat",
  "osc",
  "range",
  "phase",
  "step",
  "clear",
  "ambient",
  "directional",
  "hemisphere",
  "point",
  "rectarea",
  "spot",
  "for",
  "map",
  "sin",
  "box"
  ]

tokenParser :: GenTokenParser String Identity
tokenParser = makeTokenParser $ LanguageDef (unGenLanguageDef emptyDef) {
<<<<<<< HEAD
  reservedNames = reservedNamesDef,
=======
  reservedNames = ["dancer","camera","plane","osc","range","phase","step","clear","ambient","directional","hemisphere","point","rectarea","spot","for","map","box"],
>>>>>>> c076ef4 (box element added)
  reservedOpNames = [";","=","*","+","-","/","\\",".."],
  commentStart = "{-",
  commentEnd = "-}",
  commentLine = "--",
  nestedComments = true
  }

angles :: forall a. P a -> P a
angles = tokenParser.angles

braces :: forall a. P a -> P a
braces = tokenParser.braces

brackets :: forall a. P a -> P a
brackets = tokenParser.brackets

charLiteral :: P Char
charLiteral = tokenParser.charLiteral

colon :: P String
colon = tokenParser.colon

comma :: P String
comma = tokenParser.comma

commaSep :: forall a. P a -> P (List a)
commaSep = tokenParser.commaSep

commaSep1 :: forall a. P a -> P (NonEmptyList a)
commaSep1 = tokenParser.commaSep1

decimal :: P Int
decimal = tokenParser.decimal

dot :: P String
dot = tokenParser.dot

float :: P Number
float = tokenParser.float

hexadecimal :: P Int
hexadecimal = tokenParser.hexadecimal

identifier :: P String
identifier = tokenParser.identifier

integer :: P Int
integer = tokenParser.integer

lexeme :: forall a. P a -> P a
lexeme = tokenParser.lexeme

natural :: P Int
natural = tokenParser.natural

naturalOrFloat :: P (Either Int Number)
naturalOrFloat = tokenParser.naturalOrFloat

octal :: P Int
octal = tokenParser.octal

operator :: P String
operator = tokenParser.operator

parens :: forall a. P a -> P a
parens = tokenParser.parens

reserved :: String -> P Unit
reserved = tokenParser.reserved

reservedOp :: String -> P Unit
reservedOp = tokenParser.reservedOp

semi :: P String
semi = tokenParser.semi

semiSep :: forall a. P a -> P (List a)
semiSep = tokenParser.semiSep

semiSep1 :: forall a. P a -> P (NonEmptyList a)
semiSep1 = tokenParser.semiSep1

stringLiteral :: P String
stringLiteral = tokenParser.stringLiteral

symbol :: String -> P String
symbol = tokenParser.symbol

whiteSpace :: P Unit
whiteSpace = tokenParser.whiteSpace
