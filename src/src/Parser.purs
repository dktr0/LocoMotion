module Parser (
  Program,
  parseProgram
  ) where

import Prelude (bind, pure, show, ($), (<>), (>>=))
import Data.Map (insert,empty)
import Data.List (List, foldl, mapMaybe)
import Data.Either (Either)
import Data.Maybe (Maybe(..))
import Parsing (Position(..),ParseError(..),runParser)
import Data.Traversable (traverse)
import Control.Monad.Error.Class (throwError)
import Data.Bifunctor (lmap)

import Value
import AST (AST,Expression,Statement)
import AST as AST

type P a = StateT ValueMap (Either ParseError) a

type Program = R Unit

astToProgram :: AST -> P Program
astToProgram ast = ...

statementToProgram :: Statement -> P Program
statementToProgram (Assignment p k e) = do
  v <- expressionToValue p e
  modify_ $ \s -> s { semiGlobalMap = insert k v s.semiGlobalMap }
  pure $ valueToProgram v
statementToProgram (Action p e) = do
  v <- expressionToValue p e
  pure $ valueToProgram v

performValue :: Value -> Program
-- these value subtypes yield Program-s that actually do things
performValue (ValueDancer x) = performDancer x -- to be defined in R
performValue (ValueFloor x) = performFloor x -- to be defined in R
performValue (ValueCamera x) = performCamera x -- to be defined in R
-- all other values yield a Program that does nothing
performValue _ = pure unit

-- because expressionToValue is in the monad P it can access previous semiGlobal assignments
expressionToValue :: Position -> Expression -> P Value

... working here transferring work on previous expressionToValue here and to new approach

-- needs to be updated to new approach:
parseProgram :: String -> Either String Program
parseProgram x = lmap showParseError $ runParser x AST.ast >>= astToProgram

showParseError :: ParseError -> String
showParseError (ParseError e (Position p)) = show p.line <> ":" <> show p.column <> " " <> e
