module Parser (parseProgram,durPropertyParser) where

import Prelude
import Data.Identity
import Data.List
import Data.List.NonEmpty
import Data.Map
import Data.Either
import Data.Maybe (Maybe)
import Data.Number (fromString)
import Data.Int (toNumber)
import Control.Bind
import Parsing
import Parsing.Language (emptyDef)
import Parsing.Token (GenLanguageDef(..),LanguageDef,unGenLanguageDef,TokenParser,GenTokenParser,makeTokenParser)
import Parsing.Combinators
import Parsing.String (eof)

import AST
import Variable

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
  xs <- semiSep statement
  eof
  pure $ fromFoldableWithIndex xs

statement :: P Statement
statement = choice [
  (Dancer <$> dancer) <|>
  (Floor <$> floor) <|>
  (Camera <$> camera) <|>
  emptyStatement
  ]

emptyStatement :: P Statement
emptyStatement = do
  lookAhead whiteSpace
  lookAhead eof <|> lookAhead (reservedOp ";")
  pure EmptyStatement


dancer :: P Dancer
dancer = choice [
  try $ dancerWithProperties,
  reserved "dancer" $> defaultDancer
  ]

dancerWithProperties :: P Dancer
dancerWithProperties = do
  reserved "dancer"
  f <- dancerPropertiesParser
  pure $ f defaultDancer

dancerPropertiesParser :: P (Dancer -> Dancer)
dancerPropertiesParser = do
  reservedOp "{"
  fs <- commaSep dancerPropertyParser -- :: List (Dancer -> Dancer)
  reservedOp "}"
  pure $ foldl (>>>) identity fs

dancerPropertyParser :: P (Dancer -> Dancer)
dancerPropertyParser = choice [ posRotScaleParser, urlPropertyParser, animationPropertyParser, durPropertyParser ]

-- positionPropertyParser :: forall r. P (r -> r)
posRotScaleParser = do
  f <- choice [
    reserved "x" $> setPosX,
    reserved "y" $> setPosY,
    reserved "z" $> setPosZ,
    reserved "rx" $> setRotX,
    reserved "ry" $> setRotY,
    reserved "rz" $> setRotZ,
    reserved "sx" $> setScaleX,
    reserved "sy" $> setScaleY,
    reserved "sz" $> setScaleZ,
    reserved "size" $> setSize
    ]
  reservedOp "="
  v <- variable
  pure $ f v

-- there has to be a more elegant way of getting these setters, though, right?
setPosX n r = r { pos = r.pos { x = n } }
setPosY n r = r { pos = r.pos { y = n } }
setPosZ n r = r { pos = r.pos { z = n } }
setRotX n r = r { rot = r.rot { x = n } }
setRotY n r = r { rot = r.rot { y = n } }
setRotZ n r = r { rot = r.rot { z = n } }
setScaleX n r = r { scale = r.scale { x = n } }
setScaleY n r = r { scale = r.scale { y = n } }
setScaleZ n r = r { scale = r.scale { z = n } }
setSize n r = r { scale = {
  x: Product r.scale.x n,
  y: Product r.scale.y n,
  z: Product r.scale.z n
  }}

-- urlPropertyParser :: forall r. P (r -> r)
urlPropertyParser = do
  reserved "url"
  reservedOp "="
  x <- stringLiteral
  pure $ \r -> r { url = x }


durPropertyParser :: forall a b. P ({ dur :: a | b } -> { dur :: Variable | b })
durPropertyParser = do
  reserved "dur"
  reservedOp "="
  x <- variable
  pure $ \r -> r { dur = x }

animationPropertyParser = do
  reserved "animation"
  reservedOp "="
  x <- integer
  pure $ \r -> r { animation = x }

floor :: P Floor
floor = choice [
  try $ floorWithProperties,
  reserved "floor" $> defaultFloor
  ]

floorWithProperties :: P Floor
floorWithProperties = do
  reserved "floor"
  f <- floorPropertiesParser
  pure $ f defaultFloor

floorPropertiesParser :: P (Floor -> Floor)
floorPropertiesParser = do
  reservedOp "{"
  fs <- commaSep floorPropertyParser -- :: List (Floor -> Floor)
  reservedOp "}"
  pure $ foldl (>>>) identity fs

floorPropertyParser :: P (Floor -> Floor)
floorPropertyParser = choice [
  colourParser,
  shadowsParser
  ]

colourParser = do
  ((try $ reserved "colour") <|> reserved "color")
  reservedOp "="
  c <- integer
  pure $ \r -> r { colour=c }

shadowsParser = do
  reserved "shadows"
  reservedOp "="
  b <- boolean
  pure $ \r -> r { shadows=b }

boolean :: P Boolean
boolean = choice [
  reserved "true" $> true,
  reserved "false" $> false
  ]

camera :: P (List Camera)
camera = choice [
  try $ cameraWithProperties,
  reserved "camera" $> Nil
  ]

cameraWithProperties :: P (List Camera)
cameraWithProperties = do
  reserved "camera"
  reservedOp "{"
  fs <- commaSep cameraPropertyParser
  reservedOp "}"
  pure $ fs

cameraPropertyParser :: P Camera
cameraPropertyParser = choice [
  reserved "x" *> reservedOp "=" *> (CameraX <$> variable),
  reserved "y" *> reservedOp "=" *> (CameraY <$> variable),
  reserved "z" *> reservedOp "=" *> (CameraZ <$> variable),
  reserved "rx" *> reservedOp "=" *> (CameraRotX <$> variable),
  reserved "ry" *> reservedOp "=" *> (CameraRotY <$> variable),
  reserved "rz" *> reservedOp "=" *> (CameraRotZ <$> variable)
  ]


-- parsing of Variable-s

variable :: P Variable
variable = do
  _ <- pure unit
  chainl1 variable' additionSubtraction

additionSubtraction :: P (Variable -> Variable -> Variable)
additionSubtraction = choice [
  reservedOp "+" $> Sum,
  reservedOp "-" $> Difference
  ]

variable' :: P Variable
variable' = do
  _ <- pure unit
  chainl1 variable'' multiplicationDivision

multiplicationDivision :: P (Variable -> Variable -> Variable)
multiplicationDivision = choice [
  reservedOp "*" $> Product,
  reservedOp "/" $> Divide
  ]

variable'' :: P Variable
variable'' = do
  _ <- pure unit
  choice [
    parens variable,
    try $ Constant <$> number,
    try $ variableOsc,
    try $ variableRange
    ]

variableOsc :: P Variable
variableOsc = do
  reserved "osc"
  f <- variableAsArgument
  pure $ Osc f

variableRange :: P Variable
variableRange = do
  reserved "range"
  r1 <- variableAsArgument
  r2 <- variableAsArgument
  x <- variableAsArgument
  pure $ Range r1 r2 x

variableAsArgument :: P Variable
variableAsArgument = do
  _ <- pure unit
  choice [
    parens variable,
    try $ Constant <$> number
    ]


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

tokenParser :: GenTokenParser String Identity
tokenParser = makeTokenParser $ LanguageDef (unGenLanguageDef emptyDef) {
  reservedNames = ["dancer","camera","floor","x","y","z","url","animation","dur","rx","ry","rz","sx","sy","sz","size","osc"],
  reservedOpNames = [";","=","*","+","-","/"],
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
