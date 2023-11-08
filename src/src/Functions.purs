module Functions where

import Prelude (bind,pure,($),(*),(+),(-),(<<<))
import Parsing (Position(..),ParseError(..))
import Data.Either (Either)
import Data.Traversable (traverse)
import Data.List (singleton)
import Control.Monad.Error.Class (throwError)
import Data.Int as Int
import Data.Number as Number

import Variable
import Value

-- hsv :: Value -> Value -> Value -> Value
-- hsv (ValueVariable h) (ValueVariable s) (ValueVariable v) =


osc :: Value -> Value
osc f = ValueVariable $ Osc (valueToVariable f) (ConstantVariable 0.0)

oscphs :: Value -> Value -> Value
oscphs f phs = ValueVariable $ Osc (valueToVariable f) (valueToVariable phs)

range :: Value -> Value -> Value -> Value
range r1 r2 x = ValueVariable $ f (valueToVariable r1) (valueToVariable r2) (valueToVariable x)
  where f r1' r2' x' = (x' * ConstantVariable 0.5 + ConstantVariable 0.5) * (r2' - r1') + r1'

phase :: Value -> Value -> Value
phase dur offset = ValueVariable $ Phase (valueToVariable dur) (valueToVariable offset)

step :: Value -> Value -> Value
step xs phs = ValueVariable $ Step (valueToListVariable xs) (valueToVariable phs)

for :: Value
for = ValueFunction $ \p xs -> pure $ ValueFunction (\_ f -> fmapValue p f xs)

map :: Value
map = ValueFunction $ \p f -> pure $ ValueFunction (\_ xs -> fmapValue p f xs)

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

sin :: Value -> Value
sin (ValueVariable x) = ValueVariable $ Sin x
sin x = ValueNumber $ Number.sin $ valueToNumber x
