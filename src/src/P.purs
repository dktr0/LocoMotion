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
  lambdaMap :: ValueMap,
  program :: Program
  }

type P a = StateT PState (Either ParseError) a

runP :: forall a. P a -> Either ParseError a
runP = evalP empty empty empty defaultProgram

evalP :: forall a. ValueMap -> ValueMap -> ValueMap -> Program -> P a -> Either ParseError a
evalP sm tm lm prog p = evalStateT p { semiMap: sm, thisMap: tm, lambdaMap: lm, program: prog }

liftEitherParseError :: forall a. Either ParseError a -> P a
liftEitherParseError (Left pe) = throwError pe
liftEitherParseError (Right a) = pure a

