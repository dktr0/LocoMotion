module Program where

import Prelude
import Value
import Data.Tuple (Tuple(..))
import Data.Map (fromFoldable)

type Program = {
  dancers :: Array ValueMap,
  floors :: Array ValueMap,
  cameraMap :: ValueMap
  }

defaultProgram :: Program
defaultProgram = {
  dancers: [],
  floors: [],
  cameraMap: defaultCamera
  }

defaultDancer :: ValueMap
defaultDancer = fromFoldable [
  Tuple "x" (ValueNumber 0.0),
  Tuple "y" (ValueNumber 0.0),
  Tuple "z" (ValueNumber 0.0),
  Tuple "rx" (ValueNumber 0.0),
  Tuple "ry" (ValueNumber 0.0),
  Tuple "rz" (ValueNumber 0.0),
  Tuple "sx" (ValueNumber 1.0),
  Tuple "sy" (ValueNumber 1.0),
  Tuple "sz" (ValueNumber 1.0),
  Tuple "size" (ValueNumber 1.0)
  ]

defaultFloor :: ValueMap
defaultFloor = fromFoldable [
  Tuple "colour" (ValueInt 0x888888),
  Tuple "shadows" (ValueBoolean true)
  ]

defaultCamera :: ValueMap
defaultCamera = fromFoldable [
  Tuple "x" (ValueNumber 0.0),
  Tuple "y" (ValueNumber 1.0),
  Tuple "z" (ValueNumber 10.0),
  Tuple "rx" (ValueNumber 0.0),
  Tuple "ry" (ValueNumber 0.0),
  Tuple "rz" (ValueNumber 0.0)
  ]
