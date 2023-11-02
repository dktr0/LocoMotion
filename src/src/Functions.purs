module Functions where

import Prelude (bind,pure,($),(*),(+),(-))
import Parsing (Position(..),ParseError(..))
import Data.Either (Either)
import Data.Traversable (traverse)
import Data.List (singleton)
import Control.Monad.Error.Class (throwError)
import Data.Int as Int

import Variable
import Value


osc :: Position -> Value -> Either ParseError Value
osc _ (ValueVariable f) = pure $ ValueVariable $ Osc f
osc _ (ValueNumber f) = pure $ ValueVariable $ Osc $ ConstantVariable f
osc _ (ValueInt f) = pure $ ValueVariable $ Osc $ ConstantVariable $ Int.toNumber f
osc p _ = throwError $ ParseError "argument to osc must be Variable/Number/Int" p

range :: Position -> Value -> Either ParseError Value
range _ r1 = pure $ ValueFunction (\_ r2 -> pure $ ValueFunction (\_ x -> pure $ ValueVariable $ f (valueToVariable r1) (valueToVariable r2) (valueToVariable x)))
  where f r1' r2' x' = (x' * ConstantVariable 0.5 + ConstantVariable 0.5) * (r2' - r1') + r1'

phase :: Position -> Value -> Either ParseError Value
phase _ dur = pure $ ValueFunction (\_ offset -> pure $ ValueVariable $ Phase (valueToVariable dur) (valueToVariable offset))

step :: Position -> Value -> Either ParseError Value
step _ xs = pure $ ValueFunction (\_ phs -> pure $ ValueVariable $ Step (valueToListVariable xs) (valueToVariable phs))

for :: Position -> Value -> Either ParseError Value
for p xs = pure $ ValueFunction (\_ f -> fmapValue p f xs)

map :: Position -> Value -> Either ParseError Value
map p f = pure $ ValueFunction (\_ xs -> fmapValue p f xs)

fmapValue :: Position -> Value -> Value -> Either ParseError Value
-- main scenario is a function and a list
fmapValue p (ValueFunction f) (ValueList xs) = do
  xs' <- traverse (f p) xs
  pure $ ValueList xs'
-- if we have a function and a non-list value, wrap it in a singleton list
fmapValue p (ValueFunction f) x = fmapValue p (ValueFunction f) $ ValueList $ singleton x
-- if first argument is not a function, that's an error
fmapValue p _ _ = throwError $ ParseError "missing function argument to for/map" p
-- to resolve: couldn't transformer also take the place of functions in this? and if so, so could anything which implicitly contains a transformer (eg. dancer, camera, clear)


