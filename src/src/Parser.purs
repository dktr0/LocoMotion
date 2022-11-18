module Parser where

import Prelude
import Data.Map (insert,empty)
import Data.List
import Data.Tuple
import Data.Either
import Data.Maybe
import Parsing (Position,ParseError(..))
import Data.Foldable (foldl)
import Data.Traversable (traverse)
import Control.Monad.Error.Class (throwError)

import Value
import AST (AST,Expression,Statement)
import AST as AST

type Program = List Action

data Action =
  Dancer Transformer |
  Floor Transformer |
  Camera Transformer


astToProgram :: AST -> Either ParseError Program
astToProgram ast = do
  let semiMap = foldl collectSemiGlobal empty ast -- :: SemiMap
  let actionList = mapMaybe collectAction ast -- :: List Expression
  traverse (expressionToAction semiMap) actionList

collectSemiGlobal :: SemiMap -> Statement -> SemiMap
collectSemiGlobal m (AST.Assignment _ k e) = insert k e m
collectSemiGlobal m _ = m

collectAction :: Statement -> Maybe Expression
collectAction (AST.Action _ e) = Just e
collectAction _ = Nothing

expressionToAction :: SemiMap -> Expression -> Either ParseError Action
expressionToAction semiMap e = do
  v <- expressionToValue semiMap empty e
  case v of
    ValueDancer t -> pure $ Dancer t
    ValueFloor t -> pure $ Floor t
    ValueCamera t -> pure $ Camera t
    _ -> throwError $ ParseError "an Action must be a Dancer, Floor, or Camera" (AST.expressionPosition e)

{-
parseProgram :: String -> Either String Program
parseProgram x = case (runParser x program) of
  Left err -> Left $ showParseError err
  Right prog -> Right prog

showParseError :: ParseError -> String
showParseError (ParseError e (Position p)) = show p.line <> ":" <> show p.column <> " " <> e
-}
