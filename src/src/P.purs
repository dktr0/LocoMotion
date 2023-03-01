module P where

-- the P monad

import Prelude
import Effect.Unsafe (unsafePerformEffect)
import Effect.Console (log)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Map (empty)
import Data.Array as Array
import Control.Monad.State.Trans
import Parsing (ParseError(..),Position)
import Control.Monad.Error.Class (throwError)
import Data.Tuple (Tuple(..))


import Value
import Program
import ElementType

type PState = {
  semiMap :: ValueMap,
  thisMap :: ValueMap,
  program :: Program
  }

type P a = StateT PState (Either ParseError) a

runP :: forall a. P a -> Either ParseError a
runP = evalP empty empty defaultProgram

evalP :: forall a. ValueMap -> ValueMap -> Program -> P a -> Either ParseError a
evalP sm tm prog p = evalStateT p { semiMap: sm, thisMap: tm, program: prog }

liftEitherParseError :: forall a. Either ParseError a -> P a
liftEitherParseError (Left pe) = throwError pe
liftEitherParseError (Right a) = pure a

newElement :: ElementType -> ValueMap -> P Value
newElement t vm = do
  let e = Tuple t vm
  s <- modify $ \s -> s { program = s.program { elements = Array.snoc s.program.elements e } }
  let n = Array.length s.program.elements - 1
  pure $ ValueElement t n vm

modifyElement :: Int -> Transformer -> P Value
modifyElement n t = do
  s <- get
  Tuple eType newVm <- case Array.index s.program.elements n of
    Nothing -> do
      pure $ unsafePerformEffect $ log "modifyElement: this should not ever happen 1"
      pure $ Tuple Dancer empty -- note: Dancer is meaningless
    Just (Tuple eType vm) -> do
      newVm <- liftEitherParseError $ t vm
      pure $ Tuple eType newVm
  case Array.updateAt n (Tuple eType newVm) s.program.elements of
    Nothing -> do
      pure $ unsafePerformEffect $ log "modifyElement: this should not ever happen 2"
      pure unit
    Just x -> put $ s { program = s.program { elements = x } }
  pure $ ValueElement eType n newVm

modifyCamera :: Transformer -> P Value
modifyCamera t = do
  cm <- readCamera
  cm' <- liftEitherParseError $ t cm
  modify_ $ \s -> s { program = s.program { cameraMap = cm' } }
  pure ValueCamera

readCamera :: P ValueMap
readCamera = do
  s <- get
  pure s.program.cameraMap

modifyClear :: Transformer -> P Value
modifyClear t = do
  cm <- readClear
  cm' <- liftEitherParseError $ t cm
  modify_ $ \s -> s { program = s.program { clearMap = Just cm' } }
  pure ValueClear

readClear :: P ValueMap
readClear = do
  s <- get
  pure $ case s.program.clearMap of
    Just x -> x
    Nothing -> defaultClear
