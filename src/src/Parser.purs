module Parser (parseProgram) where

import Prelude
import Data.List
import Data.Map (fromFoldableWithIndex)
import Data.Either
import Parsing
import Parsing.Combinators
import Parsing.String (eof)

import TokenParser
import AST
import Transformer (transformer,valueExpr)

parseProgram :: String -> Either String Program
parseProgram x = case (runParser x program) of
  Left err -> Left $ showParseError err
  Right prog -> Right prog

showParseError :: ParseError -> String
showParseError (ParseError e (Position p)) = show p.line <> ":" <> show p.column <> " " <> e

program :: P Program
program = do
  whiteSpace
  xs <- semiSep statement
  eof
  pure $ fromFoldableWithIndex xs

statement :: P Statement
statement = try dancer <|> try floor <|> try camera <|> try assignment <|> emptyStatement

dancer :: P Statement
dancer = choice [
  try $ do
    reserved "dancer"
    t <- transformer
    pure $ Dancer t,
  reserved "dancer" $> Dancer Nil
  ]

floor :: P Statement
floor = choice [
  try $ do
    reserved "floor"
    t <- transformer
    pure $ Floor t,
  reserved "floor" $> Floor Nil
  ]

camera :: P Statement
camera = choice [
  try $ do
    reserved "camera"
    t <- transformer
    pure $ Camera t,
  reserved "camera" $> Camera Nil
  ]

assignment :: P Statement
assignment = do
  k <- identifier
  reservedOp "="
  v <- valueExpr
  pure $ Assignment k v

emptyStatement :: P Statement
emptyStatement = do
  lookAhead whiteSpace
  lookAhead eof <|> lookAhead (reservedOp ";")
  pure EmptyStatement
