module Parser (parseProgram,parseProgramDebug) where

import Prelude
import Effect
import Effect.Console (log)
import Data.Number (sin,pi)
import Data.Int (toNumber)
import Data.Map (insert,empty,lookup,Map(..))
import Data.Map (fromFoldable) as Map
import Data.List (List, foldl, mapMaybe, fromFoldable)
import Data.Tuple (Tuple(..))
import Data.Either (Either)
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Parsing (Position(..),ParseError(..),runParser)
import Data.Traversable (traverse_,traverse,sequence)
import Control.Monad.Error.Class (throwError)
import Control.Monad.State.Trans (get,modify_)
import Data.Bifunctor (lmap)

import Value
import Value as Value
import Variable
import AST (AST,Expression,Statement)
import AST as AST
import R (R)
import P
import Program


parseProgram :: String -> Either String Program
parseProgram x = lmap showParseError $ AST.parseAST x >>= (astToProgram >>> runP)

parseProgramDebug :: String -> Effect (Either String Program)
parseProgramDebug x = do
  let y = AST.parseAST x
  log "result of parseAST: "
  log $ show y
  case y of
    Right y' -> do
     let z = runP $ astToProgram y'
     log "result of astToProgram: "
     log $ show z
     pure $ lmap showParseError z
    Left e -> pure (Left $ showParseError e)

showParseError :: ParseError -> String
showParseError (ParseError e (Position p)) = show p.line <> ":" <> show p.column <> " " <> e

astToProgram :: AST -> P Program
astToProgram ast = do
 traverse_ parseStatement ast
 s <- get
 pure s.program

parseStatement :: Statement -> P Unit
parseStatement (AST.Assignment _ k e) = do -- position is currently unused, but it might be used in future if we were checking validity of definition names
  v <- expressionToValue e
  modify_ $ \s -> s { semiMap = insert k v s.semiMap }
parseStatement (AST.Action e) = do
  _ <- expressionToValue e
  pure unit
parseStatement (AST.EmptyStatement _) = pure unit

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
expressionToValue (AST.Element _ t) = newElement t
expressionToValue (AST.Camera _) = pure ValueCamera
expressionToValue (AST.Clear _) = pure ValueClear
expressionToValue (AST.Osc _) = pure $ ValueFunction oscFunction
expressionToValue (AST.Range _) = pure $ ValueFunction rangeFunction
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

newElement ::

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
        ValueDancer _ vmX -> applyTransformer tF vmX
        ValueFloor _ vmX -> applyTransformer tF vmX
        ValueCamera -> do
          vmX <- readCamera
          applyTransformer tF vmX
        ValueClear -> do
          vmX <- readClear
          applyTransformer tF vmX
        _ -> throwError $ ParseError "invalid argument applied to Transformer" (AST.expressionPosition eX)
    ValueDancer i _ -> do
      case x of
        ValueTransformer tX -> modifyDancer i tX
        ValueDancer _ vmX -> modifyDancer i (valueMapToTransformer vmX)
        ValueFloor _ vmX -> modifyDancer i (valueMapToTransformer vmX)
        ValueCamera -> do
          vmX <- readCamera
          modifyDancer i (valueMapToTransformer vmX)
        ValueClear -> do
          vmX <- readClear
          modifyDancer i (valueMapToTransformer vmX)
        _ -> throwError $ ParseError "invalid argument applied to Dancer" (AST.expressionPosition eX)
    ValueFloor i _ -> do
      case x of
        ValueTransformer tX -> modifyFloor i tX
        ValueDancer _ vmX -> modifyFloor i (valueMapToTransformer vmX)
        ValueFloor _ vmX -> modifyFloor i (valueMapToTransformer vmX)
        ValueCamera -> do
          vmX <- readCamera
          modifyFloor i (valueMapToTransformer vmX)
        ValueClear -> do
          vmX <- readClear
          modifyFloor i (valueMapToTransformer vmX)
        _ -> throwError $ ParseError "invalid argument applied to Floor" (AST.expressionPosition eX)
    ValueCamera -> do
      case x of
        ValueTransformer tX -> modifyCamera tX
        ValueDancer _ vmX -> modifyCamera (valueMapToTransformer vmX)
        ValueFloor _ vmX -> modifyCamera (valueMapToTransformer vmX)
        ValueCamera -> pure ValueCamera
        ValueClear -> do
          vmX <- readClear
          modifyCamera (valueMapToTransformer vmX)
        _ -> throwError $ ParseError "invalid argument applied to Camera" (AST.expressionPosition eX)
    ValueClear -> do
      case x of
        ValueTransformer tX -> modifyClear tX
        ValueDancer _ vmX -> modifyClear (valueMapToTransformer vmX)
        ValueFloor _ vmX -> modifyClear (valueMapToTransformer vmX)
        ValueCamera -> do
          vmX <- readCamera
          modifyClear (valueMapToTransformer vmX)
        ValueClear -> pure ValueClear
        _ -> throwError $ ParseError "invalid argument applied to Clear" (AST.expressionPosition eX)
    ValueFunction f' -> do
      case f' (AST.expressionPosition eX) x of
        Left pe -> throwError pe
        Right v -> pure v


-- Miscellaneous functions

oscFunction :: Position -> Value -> Either ParseError Value
oscFunction _ (ValueVariable f) = pure $ ValueVariable $ Osc f
oscFunction _ (ValueNumber f) = pure $ ValueVariable $ Osc $ ConstantVariable f
oscFunction _ (ValueInt f) = pure $ ValueVariable $ Osc $ ConstantVariable $ toNumber f
oscFunction p _ = throwError $ ParseError "argument to osc must be Variable/Number/Int" p

rangeFunction :: Position -> Value -> Either ParseError Value
rangeFunction _ r1 = pure $ ValueFunction (\_ r2 -> pure $ ValueFunction (\_ x -> pure $ ValueVariable $ f (valueToVariable r1) (valueToVariable r2) (valueToVariable x)))
  where f r1' r2' x' = (x' * ConstantVariable 0.5 + ConstantVariable 0.5) * (r2' - r1') + r1'


-- Transformers

transformerToValue :: List (Tuple String Expression) -> P Value
transformerToValue xs = do
  ts <- traverse parseModifier xs -- :: P (List Transformer)
  pure $ ValueTransformer $ foldl appendTransformers pure ts

parseModifier :: Tuple String Expression -> P Transformer
parseModifier (Tuple k e) = do
  s <- get
  pure $ \tm -> evalP s.semiMap tm s.program $ do
    v <- expressionToValue e
    pure $ insert k v tm

applyTransformer :: Transformer -> ValueMap -> P Value -- where Value is always a Transformer
applyTransformer tF x =
  case tF x of
    Left pe -> throwError pe
    Right vm -> pure $ ValueTransformer $ valueMapToTransformer vm
