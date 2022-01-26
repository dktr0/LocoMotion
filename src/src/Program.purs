module Program (
  Program(..),
  defaultProgram,
  parseProgram
  ) where

import Prelude
import Data.Identity
import Data.List
import Data.List.NonEmpty
import Data.Either
import Data.Maybe (Maybe)
import Data.Number (fromString)
import Control.Bind
import Text.Parsing.Parser
import Text.Parsing.Parser.Pos
import Text.Parsing.Parser.Language (emptyDef)
import Text.Parsing.Parser.Token (GenLanguageDef(..),LanguageDef,unGenLanguageDef,TokenParser,GenTokenParser,makeTokenParser)
import Text.Parsing.Parser.Combinators


-- Program-s are semi-colon separated lists of Statement-s
-- (will rename to Program when we refactor parser in next step)

data Program' = List Statement

-- Statement-s describe an Element in the scene
-- or they change some other parameter of the rendering (eg. camera settings)

data Statement =
  Element Element |
  CameraChange (Camera -> Camera)

-- Element-s can be Dancer-s (which can be animated, have physics, etc)
-- or Ethereal-s (which are displayed but do not have physics [maybe they might have animation, though?])

data Element =
  Dancer Dancer |
  Ethereal Ethereal

type Dancer = {
  url :: String,
  animation :: Int,
  position :: Vec3
  }

-- and for now Ethereals are just polarGridHelpers...

data Ethereal = PolarGridHelper {
  radius :: Number, -- three.js default is 10, must be positive
  radials :: Int, -- three.js default is 16, must be positive
  circles :: Int, -- three.js default is 8, must be positive
  divisions :: Int, -- three.js default is 64, must be 3 or greater
  position :: Vec3
  }

type Camera = {
  position :: Vec3,
  rotation :: Vec3
  }

-- normally I wouldn't use x,y,z as the name of a record field
-- but perhaps this is more okay with purescript? Let's see...

type Vec3 = {
  x :: Number,
  y :: Number,
  z :: Number
  }

data Program = Stationary | Fast | Slow

defaultProgram :: Program
defaultProgram = Stationary

parseProgram :: String -> Either String Program
parseProgram x = case (runParser x program) of
  Left err -> Left $ showParseError err
  Right prog -> Right prog

showParseError :: ParseError -> String
showParseError (ParseError e (Position p)) = show p.line <> ":" <> show p.column <> " " <> e

type P a = ParserT String Identity a

program :: P Program
program = choice [
  reserved "stationary" $> Stationary,
  reserved "fast" $> Fast,
  reserved "slow" $> Slow
  ]

tokenParser :: GenTokenParser String Identity
tokenParser = makeTokenParser $ LanguageDef (unGenLanguageDef emptyDef) {
  reservedNames = ["stationary","fast","slow"]
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
