module ElementType where

import Prelude
import Data.Generic.Rep
import Data.Show.Generic

data ElementType =
  Dancer |
  Floor |
  Ambient |
  Directional |
  Hemisphere |
  Point |
  RectArea |
  Spot

derive instance Generic ElementType _

derive instance Eq ElementType

instance Show ElementType where
  show = genericShow

isLight :: ElementType -> Boolean
isLight Ambient = true
isLight Directional = true
isLight Hemisphere = true
isLight Point = true
isLight RectArea = true
isLight Spot = true
isLight _ = false
