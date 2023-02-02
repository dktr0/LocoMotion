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


import Value
import Program

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

newDancer :: P Value
newDancer = do
  let vm = defaultDancer
  s <- modify $ \s -> s { program = s.program { dancers = Array.snoc s.program.dancers vm } }
  let n = Array.length s.program.dancers - 1
  pure $ ValueDancer n vm

modifyDancer :: Int -> Transformer -> P Value
modifyDancer n t = do
  s <- get
  mNew <- case Array.index s.program.dancers n of
    Nothing -> do
      pure $ unsafePerformEffect $ log "modifyDancer: this should not ever happen 1"
      pure empty
    Just m -> liftEitherParseError $ t m
  case Array.updateAt n mNew s.program.dancers of
    Nothing -> do
      pure $ unsafePerformEffect $ log "modifyDancer: this should not ever happen 2"
      pure unit
    Just x -> put $ s { program = s.program { dancers = x } }
  pure $ ValueDancer n mNew

newFloor :: P Value
newFloor = do
  let vm = defaultFloor
  s <- modify $ \s -> s { program = s.program { floors = Array.snoc s.program.floors vm } }
  let n = Array.length s.program.floors - 1
  pure $ ValueFloor n vm

modifyFloor :: Int -> Transformer -> P Value
modifyFloor n t = do
  s <- get
  mNew <- case Array.index s.program.floors n of
    Nothing -> do
      pure $ unsafePerformEffect $ log "modifyFloor: this should not ever happen 1"
      pure empty
    Just m -> liftEitherParseError $ t m
  case Array.updateAt n mNew s.program.floors of
    Nothing -> do
      pure $ unsafePerformEffect $ log "modifyFloor: this should not ever happen 2"
      pure unit
    Just x -> put $ s { program = s.program { floors = x } }
  pure $ ValueFloor n mNew

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
