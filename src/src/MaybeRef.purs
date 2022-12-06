module MaybeRef where

import Prelude (Unit, bind, pure, unit)
import Effect (Effect)
import Effect.Ref (Ref, read)
import Data.Maybe (Maybe(..))

type MaybeRef a = Ref (Maybe a)

-- like 'when' but specialized for a MaybeRef
whenMaybeRef :: forall a. MaybeRef a -> (a -> Effect Unit) -> Effect Unit
whenMaybeRef mRef f = do
  m <- read mRef
  case m of
    Just a -> f a
    Nothing -> pure unit
