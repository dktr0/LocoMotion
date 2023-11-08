module Parser (parseProgram) where

import Prelude
import Effect
import Effect.Console (log)
import Data.Number (sin,pi)
import Data.Int (toNumber)
import Data.Map (insert,empty,lookup,Map(..))
import Data.Map (fromFoldable) as Map
import Data.List (List(..), foldl, mapMaybe, fromFoldable, singleton, (:))
import Data.Tuple (Tuple(..),fst)
import Data.Either (Either)
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Data.Foldable (elem)
import Parsing (Position(..),ParseError(..),runParser)
import Data.Traversable (traverse_,traverse,sequence)
import Control.Monad.Error.Class (throwError)
import Control.Monad.State.Trans (get,modify_)
import Data.Bifunctor (lmap)
import Control.Monad.State.Trans (evalStateT)
import Data.Array as Array

import Value
import Value as Value
import Variable
import AST (AST,Expression,Statement)
import AST as AST
import R (R)
import P
import Program
import ElementType
import Functions as Functions

parseProgram :: String -> Either String Program
parseProgram x = lmap showParseError $ AST.parseAST x >>= (astToProgram >>> runP)

{-
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
-}

showParseError :: ParseError -> String
showParseError (ParseError e (Position p)) = show p.line <> ":" <> show p.column <> " " <> e

astToProgram :: AST -> P Program
astToProgram ast = do
 traverse_ parseStatement ast
 s <- get
 pure $ setCustomLightsFlag s.program

setCustomLightsFlag :: Program -> Program
setCustomLightsFlag p = p { hasCustomLights = elem true $ map (isLight <<< fst) p.elements }

parseStatement :: Statement -> P Unit
parseStatement (AST.Assignment _ name args body) = do
  v <- embedLambdas args body
  modify_ $ \s -> s { semiMap = insert name v s.semiMap }
  valueToEffect v
parseStatement (AST.Action e) = do
  v <- expressionToValue e
  valueToEffect v
parseStatement (AST.EmptyStatement _) = pure unit


valueToEffect :: Value -> P Unit
valueToEffect (ValueElement Dancer f) = elementToEffect Dancer defaultDancer f
valueToEffect (ValueElement Plane f) = elementToEffect Plane defaultPlane f
valueToEffect (ValueElement Box f) = elementToEffect Box defaultBox f
valueToEffect (ValueElement Ambient f) = lightToEffect Ambient empty f
valueToEffect (ValueElement Directional f) = lightToEffect Directional empty f
valueToEffect (ValueElement Hemisphere f) = lightToEffect Hemisphere empty f
valueToEffect (ValueElement Point f) = lightToEffect Point empty f
valueToEffect (ValueElement RectArea f) = lightToEffect RectArea empty f
valueToEffect (ValueElement Spot f) = lightToEffect Spot empty f
valueToEffect (ValueCamera f) = modify_ $ \s -> s { program = s.program { camera = appendTransformers s.program.camera f }}
valueToEffect (ValueClear f) = modify_ $ \s -> s { program = s.program { clear = appendTransformers s.program.clear f }}
valueToEffect (ValueList xs) = traverse_ valueToEffect xs
valueToEffect _ = pure unit

elementToEffect :: ElementType -> ValueMap -> Transformer -> P Unit
elementToEffect t vm f = do
  vm' <- liftEitherParseError $ f vm
  modify_ $ \s -> s { program = s.program { elements = Array.snoc s.program.elements (Tuple t vm') } }

lightToEffect :: ElementType -> ValueMap -> Transformer -> P Unit
lightToEffect t vm f = do
  vm' <- liftEitherParseError $ f vm
  modify_ $ \s -> s { program = s.program { elements = Array.snoc s.program.elements (Tuple t vm') } }
  modify_ $ \s -> s { program = s.program { hasCustomLights = true } }


expressionToValue :: Expression -> P Value
expressionToValue (AST.LiteralNumber _ x) = pure $ ValueNumber x
expressionToValue (AST.LiteralString _ x) = pure $ ValueString x
expressionToValue (AST.LiteralInt _ x) = pure $ ValueInt x
expressionToValue (AST.LiteralBoolean _ x) = pure $ ValueBoolean x
expressionToValue (AST.ListExpression _ xs) = ValueList <$> traverse expressionToValue xs
expressionToValue (AST.This p k) = do
  s <- get
  let tMap = s.thisMap
  case lookup k tMap of
    Nothing -> throwError $ ParseError ("unknown this reference: " <> k) p
    Just v -> pure v
expressionToValue (AST.Identifier p k) = do
  s <- get
  let lMap = s.lambdaMap
  case lookup k s.lambdaMap of
    Just v -> pure v
    Nothing -> do
      case lookup k s.semiMap of
        Just v -> pure v
        Nothing -> throwError $ ParseError ("reference to unknown identifier: " <> k) p
expressionToValue (AST.Application p e1 e2) = applicationToValue p e1 e2
expressionToValue (AST.Transformer _ xs) = transformerToValue xs
expressionToValue (AST.Reserved p x) = reservedToValue p x
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
expressionToValue (AST.Lambda _ xs e) = embedLambdas xs e


reservedToValue :: Position -> String -> P Value
reservedToValue _ "dancer" = pure $ ValueElement Dancer emptyTransformer
reservedToValue _ "plane" = pure $ ValueElement Plane emptyTransformer
reservedToValue _ "box" = pure $ ValueElement Box emptyTransformer
reservedToValue _ "ambient" = pure $ ValueElement Ambient emptyTransformer
reservedToValue _ "directional" = pure $ ValueElement Directional emptyTransformer
reservedToValue _ "hemisphere" = pure $ ValueElement Hemisphere emptyTransformer
reservedToValue _ "point" = pure $ ValueElement Point emptyTransformer
reservedToValue _ "rectarea" = pure $ ValueElement RectArea emptyTransformer
reservedToValue _ "spot" = pure $ ValueElement Spot emptyTransformer
reservedToValue _ "camera" = pure $ ValueCamera emptyTransformer
reservedToValue _ "clear" = pure $ ValueClear emptyTransformer
reservedToValue _ "cps" = pure $ ValueVariable CPS
reservedToValue _ "cycle" = pure $ ValueVariable Cycle
reservedToValue _ "time" = pure $ ValueVariable Time
reservedToValue _ "beat" = pure $ ValueVariable Beat
reservedToValue _ "osc" = pure $ valueFunction Functions.osc
reservedToValue _ "range" = pure $ valueFunction3 Functions.range
reservedToValue _ "phase" = pure $ valueFunction2 Functions.phase
reservedToValue _ "step" = pure $ valueFunction2 Functions.step
reservedToValue _ "for" = pure $ Functions.for
reservedToValue _ "map" = pure $ Functions.map
reservedToValue _ "sin" = pure $ valueFunction Functions.sin
reservedToValue p x = throwError $ ParseError ("internal LocoMotion error: reservedToValue called for unknown identifier: " <> x) p


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
    ValueList _ -> throwError $ ParseError "A List can't be the left side of a function application" p
    ValueTransformer tF -> do
      case x of
        ValueTransformer tX -> pure $ ValueTransformer $ appendTransformers tF tX
        ValueElement _ tX -> pure $ ValueTransformer $ appendTransformers tF tX
        ValueCamera tX -> pure $ ValueTransformer $ appendTransformers tF tX
        ValueClear tX -> pure $ ValueTransformer $ appendTransformers tF tX
        _ -> throwError $ ParseError "invalid argument applied to Transformer" (AST.expressionPosition eX)
    ValueElement eType tF -> do
      case x of
        ValueTransformer tX -> pure $ ValueElement eType $ appendTransformers tF tX
        ValueElement _ tX -> pure $ ValueElement eType $ appendTransformers tF tX
        ValueCamera tX -> pure $ ValueElement eType $ appendTransformers tF tX
        ValueClear tX -> pure $ ValueElement eType $ appendTransformers tF tX
        _ -> throwError $ ParseError "invalid argument applied to element" (AST.expressionPosition eX)
    ValueCamera tF -> do
      case x of
        ValueTransformer tX -> pure $ ValueCamera $ appendTransformers tF tX
        ValueElement _ tX -> pure $ ValueCamera $ appendTransformers tF tX
        ValueCamera tX -> pure $ ValueCamera $ appendTransformers tF tX
        ValueClear tX -> pure $ ValueCamera $ appendTransformers tF tX
        _ -> throwError $ ParseError "invalid argument applied to Camera" (AST.expressionPosition eX)
    ValueClear tF -> do
      case x of
        ValueTransformer tX -> pure $ ValueClear $ appendTransformers tF tX
        ValueElement _ tX -> pure $ ValueClear $ appendTransformers tF tX
        ValueCamera tX -> pure $ ValueClear $ appendTransformers tF tX
        ValueClear tX -> pure $ ValueClear $ appendTransformers tF tX
        _ -> throwError $ ParseError "invalid argument applied to Clear" (AST.expressionPosition eX)
    ValueFunction f' -> do
      case f' (AST.expressionPosition eX) x of
        Left pe -> throwError pe
        Right v -> pure v


embedLambdas :: List String -> Expression -> P Value
embedLambdas Nil e = expressionToValue e
embedLambdas (argName:moreArgs) e = do
  pState <- get
  pure $ ValueFunction (\_ argValue -> evalStateT (embedLambdas moreArgs e) (pState { lambdaMap = insert argName argValue pState.lambdaMap }))

-- Transformers

transformerToValue :: List (Tuple String Expression) -> P Value
transformerToValue xs = do
  ts <- traverse parseModifier xs -- :: P (List Transformer)
  pure $ ValueTransformer $ foldl appendTransformers pure ts

parseModifier :: Tuple String Expression -> P Transformer
parseModifier (Tuple k e) = do
  s <- get
  pure $ \tm -> evalP s.semiMap tm s.lambdaMap s.program $ do
    v <- expressionToValue e
    pure $ insert k v tm


