module Parser (
  Program,
  parseProgram
  ) where

import Prelude
import Data.Number (sin,pi)
import Data.Int (toNumber)
import Data.Map (insert,empty,lookup)
import Data.Map (fromFoldable) as Map
import Data.List (List, foldl, mapMaybe, fromFoldable)
import Data.Tuple (Tuple(..))
import Data.Either (Either)
import Data.Maybe (Maybe(..))
import Parsing (Position(..),ParseError(..),runParser)
import Data.Traversable (traverse,sequence)
import Control.Monad.Error.Class (throwError)
import Control.Monad.State.Trans (get,modify_)
import Data.Bifunctor (lmap)

import Value
import Variable
import AST (AST,Expression,Statement)
import AST as AST
import R (R)

type Program = R Unit

parseProgram :: String -> Either String Program
parseProgram x = lmap showParseError $ runParser x AST.ast >>= (astToProgram >>> runP)

showParseError :: ParseError -> String
showParseError (ParseError e (Position p)) = show p.line <> ":" <> show p.column <> " " <> e

astToProgram :: AST -> P Program
astToProgram ast = do
 ps <- traverse statementToProgram ast -- :: P (List Program)
 pure $ do
   sequence ps
   pure unit

statementToProgram :: Statement -> P (R Unit)
statementToProgram (AST.Assignment _ k e) = do -- position is currently unused, but it might be used in future if we were checking validity of definition names
  v <- expressionToValue empty e
  modify_ $ \s -> s { semiGlobals = insert k v s.semiGlobals }
  pure $ performValue v
statementToProgram (AST.Action e) = do
  v <- expressionToValue empty e
  pure $ performValue v

performValue :: Value -> R Unit
performValue (ValueDancer r t) = performDancer r t
performValue (ValueFloor r t) = performFloor r t
performValue (ValueCamera t) = performCamera t
performValue _ = pure unit -- all other values yield a Program that does nothing

-- performDancer Floor and Camera are placeholders - these would be defined elsewhere
performDancer :: Int -> Transformer -> R Unit
performDancer _ _ = pure unit

performFloor :: Int -> Transformer -> R Unit
performFloor _ _ = pure unit

performCamera :: Transformer -> R Unit
performCamera _ = pure unit


expressionToValue :: Expression -> P Value
expressionToValue (AST.LiteralNumber _ x) = pure $ ValueNumber x
expressionToValue (AST.LiteralString _ x) = pure $ ValueString x
expressionToValue (AST.LiteralInt _ x) = pure $ ValueInt x
expressionToValue (AST.LiteralBoolean _ x) = pure $ ValueBoolean x
expressionToValue (AST.This p k) = do
  s <- get
  let tMap = s.thisMap
  case lookup k tMap of
    Nothing -> throwError $ ParseError ("unknown this reference " <> k) p
    Just v -> pure v
expressionToValue (AST.SemiGlobal p k) = do
  s <- get
  let sMap = s.semiMap
  case lookup k sMap of
    Nothing -> throwError $ ParseError ("unknown semiglobal reference " <> k) p
    Just v -> pure v
expressionToValue (AST.Application p e1 e2) = applicationToValue p e1 e2
expressionToValue (AST.Transformer _ xs) = transformerToValue xs
expressionToValue (AST.Dancer _) = do
  n <- newRef
  pure $ ValueDancer n defaultDancerTransformer
expressionToValue (AST.Floor _) = do
  n <- newRef
  pure $ ValueFloor n defaultFloorTransformer
expressionToValue (AST.Camera _) = pure $ ValueCamera defaultCameraTransformer
expressionToValue (AST.Osc _) = pure $ ValueFunction $ oscFunction
expressionToValue (AST.Range _) = pure $ ValueFunction $ rangeFunction
expressionToValue (AST.Sum _ e1 e2) = do
  v1 <- expressionToValue e1
  v2 <- expressionToValue e2
  pure $ v1 + v2
expressionToValue (AST.Difference _ e1 e2) = do
  v1 <- expressionToValue e1
  v2 <- expressionToValue e2
  pure $ v1 - v2
expressionToValue (AST.Product _ e1 e2) = do
  v1 <- expressionToValue e1
  v2 <- expressionToValue e2
  pure $ v1 * v2
expressionToValue (AST.Divide _ e1 e2) = do
  v1 <- expressionToValue e1
  v2 <- expressionToValue e2
  pure $ divideValues v1 v2


applicationToValue :: Position -> Expression -> Expression -> P Value
applicationToValue p eF eX = do
  f <- expressionToValue eF
  x <- expressionToValue eX
  case f of
    ValueNumber _ -> throwError $ ParseError "A Number can't be the left side of a function application" p
    ValueString _ -> throwError $ ParseError "A String can't be the left side of a function application" p
    ValueInt _ -> throwError $ ParseError "An Int can't be the left side of a function application" p
    ValueBoolean _ -> throwError $ ParseError "A Boolean can't be the left side of a function application" p
    ValueVariable _ -> throwError $ ParseError "A Variable can't be the left side of a function application" p
    ValueTransformer tF -> do
      case x of
        ValueTransformer tX -> pure $ ValueTransformer $ appendTransformers tF tX

        *** WORKING HERE  ***
        
        ValueDancer i tX -> pure $ ValueDancer i ???? $ appendTransformers tX tF -- note reverse application when transformers are on left of dancers/floor/camera
        ValueFloor i tX -> pure $ ValueFloor $ appendTransformers tX tF
        ValueCamera tX -> pure $ ValueCamera $ appendTransformers tX tF
        _ -> throwError $ ParseError "invalid argument applied to Transformer" (AST.expressionPosition eX)
    ValueDancer _ tF -> do
      case x of
        ValueTransformer tX -> pure $ ValueDancer $ appendTransformers tF tX
        ValueDancer _ tX -> pure $ ValueDancer $ appendTransformers tF tX
        _ -> throwError $ ParseError "invalid argument applied to Dancer" (AST.expressionPosition eX)
    ValueCamera tF -> do
      case x of
        ValueTransformer tX -> pure $ ValueCamera $ appendTransformers tF tX
        ValueCamera tX -> pure $ ValueCamera $ appendTransformers tF tX
        _ -> throwError $ ParseError "invalid argument applied to Camera" (AST.expressionPosition eX)
    ValueFloor _ tF -> do
      case x of
        ValueTransformer tX -> pure $ ValueFloor $ appendTransformers tF tX
        ValueFloor _ tX -> pure $ ValueFloor $ appendTransformers tF tX
        _ -> throwError $ ParseError "invalid argument applied to Floor" (AST.expressionPosition eX)
    ValueFunction f' -> f' (AST.expressionPosition eX) x


-- Miscellaneous functions

oscFunction :: Position -> Value -> Either ParseError Value
oscFunction _ (ValueVariable (Variable f)) = pure $ ValueVariable $ Variable $ \nCycles -> sin $ f nCycles * nCycles * 2.0 * pi
oscFunction _ (ValueNumber f) = pure $ ValueVariable $ Variable $ \nCycles -> sin $ f * nCycles * 2.0 * pi
oscFunction p (ValueInt f) = oscFunction p (ValueNumber $ toNumber f)
oscFunction p _ = throwError $ ParseError "argument to osc must be Variable/Number/Int" p

rangeFunction :: Position -> Value -> Either ParseError Value
rangeFunction _ r1 = pure $ ValueFunction (\_ r2 -> pure $ ValueFunction (\_ x -> pure $ ValueVariable $ f (valueToVariable r1) (valueToVariable r2) (valueToVariable x)))
  where f (Variable r1') (Variable r2') (Variable x') = Variable $ \nCycles -> (x' nCycles * 0.5 + 0.5) * (r2' nCycles - r1' nCycles) + r1' nCycles


-- Transformers

transformerToValue :: List (Tuple String Expression) -> P Value
transformerToValue xs = do
  ts <- traverse parseModifier xs -- :: P (List Transformer)
  pure $ ValueTransformer $ foldl appendTransformers pure ts

parseModifier :: Tuple String Expression -> P Transformer
parseModifier (Tuple k e) = pure $ \thisMap -> do
  v <- expressionToValue thisMap e
  pure $ insert k v thisMap




defaultCameraTransformer :: Transformer
defaultCameraTransformer = pure $ pure $ Map.fromFoldable [
  Tuple "x" (ValueNumber 0.0),
  Tuple "y" (ValueNumber 1.0),
  Tuple "z" (ValueNumber 10.0),
  Tuple "rx" (ValueNumber 0.0),
  Tuple "ry" (ValueNumber 0.0),
  Tuple "rz" (ValueNumber 0.0)
  ]
