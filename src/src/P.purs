module P where

-- the P monad

import Prelude (pure, ($))
import Data.Either (Either(..))
import Data.Map (empty)
import Control.Monad.State.Trans (StateT,evalStateT)
import Parsing (ParseError)
import Control.Monad.Except.Trans (ExceptT,runExceptT)
import Effect.Aff (Aff,throwError)

import Value (LibraryCache, ValueMap)
import Program (Program, defaultProgram)

type PState = {
  semiMap :: ValueMap,
  thisMap :: ValueMap,
  lambdaMap :: ValueMap,
  program :: Program,
  libCache :: LibraryCache
  }

type P a = StateT PState (ExceptT ParseError Aff) a

runP :: forall a. LibraryCache -> P a -> Aff (Either ParseError a)
runP lc = evalP empty empty empty defaultProgram lc

evalP :: forall a. ValueMap -> ValueMap -> ValueMap -> Program -> LibraryCache -> P a -> Aff (Either ParseError a)
evalP sm tm lm prog lc x = runExceptT $ evalStateT x { semiMap: sm, thisMap: tm, lambdaMap: lm, program: prog, libCache: lc }

liftEitherParseError :: forall a. Either ParseError a -> P a
liftEitherParseError (Left pe) = throwError pe
liftEitherParseError (Right a) = pure a
