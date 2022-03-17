module Parser (parseProgram) where

import Prelude
import Data.Identity
import Data.List
import Data.List.NonEmpty
import Data.Either
import Data.Maybe (Maybe)
import Data.Number (fromString)
import Data.Int (toNumber)
import Control.Bind
import Text.Parsing.Parser
import Text.Parsing.Parser.Pos
import Text.Parsing.Parser.Language (emptyDef)
import Text.Parsing.Parser.Token (GenLanguageDef(..),LanguageDef,unGenLanguageDef,TokenParser,GenTokenParser,makeTokenParser)
import Text.Parsing.Parser.Combinators

import AST

parseProgram :: String -> Either String Program
parseProgram x = case (runParser x program) of
  Left err -> Left $ showParseError err
  Right prog -> Right prog

showParseError :: ParseError -> String
showParseError (ParseError e (Position p)) = show p.line <> ":" <> show p.column <> " " <> e

type P a = ParserT String Identity a

program :: P Program
program = do
  whiteSpace
  sepBy statement (reservedOp ";")

statement :: P Statement
statement = choice
  [
  (Element <$> element) {- <|>
  (CameraChange <$> cameraChange) -}
  ]

element :: P Element
element = choice [
  (Dancer <$> dancer),
  (Ethereal <$> ethereal)
  ]

dancer :: P Dancer
dancer = choice [
  dancerWithProperties,
  reserved "dancer" $> defaultDancer
  ]

dancerWithProperties :: P Dancer
dancerWithProperties = do
  reserved "dancer"
  f <- dancerPropertiesParser
  pure $ f defaultDancer

-- there has to be a more elegant way of getting these setters, though, right?
setPosX n r = r { pos = r.pos { x = n } }
setPosY n r = r { pos = r.pos { y = n } }
setPosZ n r = r { pos = r.pos { z = n } }
setRotX n r = r { rot = r.rot { x = n } }
setRotY n r = r { rot = r.rot { y = n } }
setRotZ n r = r { rot = r.rot { z = n } }

-- positionPropertyParser :: forall r. P (r -> r)
posRotPropertyParser = do
  f <- choice [
    reserved "x" $> setPosX,
    reserved "y" $> setPosY,
    reserved "z" $> setPosZ,
    reserved "rx" $> setRotX,
    reserved "ry" $> setRotY,
    reserved "rz" $> setRotZ
    ]
  reservedOp "="
  n <- number
  pure $ f n

-- urlPropertyParser :: forall r. P (r -> r)
urlPropertyParser = do
  reserved "url"
  reservedOp "="
  x <- stringLiteral
  pure $ \r -> r { url = x }


dancerPropertyParser :: P (Dancer -> Dancer)
dancerPropertyParser = choice [ posRotPropertyParser, urlPropertyParser ]

dancerPropertiesParser :: P (Dancer -> Dancer)
dancerPropertiesParser = do
  reservedOp "{"
  fs <- commaSep dancerPropertyParser -- :: List (Dancer -> Dancer)
  reservedOp "}"
  pure $ foldl (>>>) identity fs


ethereal :: P Ethereal
ethereal = reserved "polarGridHelper" $> defaultEthereal


number :: P Number
number = choice [
  try $ float, -- issue here with parsing negative floats: https://github.com/purescript-contrib/purescript-parsing/pull/142
  toNumber <$> integer
  ]

tokenParser :: GenTokenParser String Identity
tokenParser = makeTokenParser $ LanguageDef (unGenLanguageDef emptyDef) {
  reservedNames = ["dancer","polarGridHelper","x","y","z","url"],
  reservedOpNames = [";","="],
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
