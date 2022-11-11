module Parser where

import Prelude
import Data.Map

import Value
import AST (AST)
import AST as AST

type Program = Map Int Action

data Action =
  Dancer Transformer |
  Floor Transformer |
  Camera Transformer

{-
astToProgram :: AST -> Either ParseError Program
astToProgram ast = do
  -- calculate map of semi-globals
  -- use that
-}

expressionToAction :: ValueMap -> Expression -> Either ParseErrror Action
expressionToAction semiMap e = do
  v <- expressionToValue semiMap empty e
  case v of
    ValueDancer t -> pure $ Dancer t
    ValueFloor t -> pure $ Floor t
    ValueCamera t -> pure $ Camera t
    _ -> throwError $ ParseError "an Action must be a Dancer, Floor, or Camera" (expressionPosition e)

{-
parseProgram :: String -> Either String Program
parseProgram x = case (runParser x program) of
  Left err -> Left $ showParseError err
  Right prog -> Right prog

showParseError :: ParseError -> String
showParseError (ParseError e (Position p)) = show p.line <> ":" <> show p.column <> " " <> e
-}
