module ZoneMap where

import Prelude
import Data.Map as Map
import Data.Maybe (Maybe)
import Effect (Effect)
import Effect.Ref as Ref

type Zone = Int

type ZoneMap a = Ref.Ref (Map.Map Zone a)

new :: forall a. Effect (ZoneMap a)
new = Ref.new $ Map.empty

read :: forall a. Zone -> ZoneMap a -> Effect (Maybe a)
read z m = do
  m' <- Ref.read m
  pure $ Map.lookup z m'

write :: forall a. Zone -> a -> ZoneMap a -> Effect Unit
write z a m = do
  m' <- Ref.read m
  Ref.write (Map.insert z a m') m

delete :: forall a. Zone -> ZoneMap a -> Effect Unit
delete z m = do
  m'<- Ref.read m
  Ref.write (Map.delete z m') m

count :: forall a. ZoneMap a -> Effect Int
count m = do
  m' <- Ref.read m
  pure $ Map.size m'
