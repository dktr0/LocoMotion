module MaybeRef where

import Prelude
import Effect (Effect)
import Effect.Class (liftEffect, class MonadEffect)
import Effect.Ref (Ref, read)
import Data.Maybe (Maybe(..))

type MaybeRef a = Ref (Maybe a)

-- like 'when' but specialized for a MaybeRef
whenMaybeRef :: forall a m. MonadEffect m => MaybeRef a -> (a -> m Unit) -> m Unit
whenMaybeRef mRef f = do
  m <- liftEffect $ read mRef
  case m of
    Just a -> f a
    Nothing -> pure unit
