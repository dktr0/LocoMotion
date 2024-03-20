module Program where

import Prelude
import Data.Tuple (Tuple(..))
import Data.Map (fromFoldable)
import Data.Maybe (Maybe(..))

import ElementType
import Value

type Program = {
  elements :: Array ProgramElement,
  camera :: Transformer,
  clear :: Transformer,
  hasCustomLights :: Boolean,
  eTime :: Number -- evaluation time in POSIX 1970 seconds
  }

type ProgramElement = Tuple ElementType ValueMap

defaultProgram :: Program
defaultProgram = {
  elements: [],
  camera: emptyTransformer,
  clear: emptyTransformer,
  hasCustomLights: false,
  eTime: 0.0
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

defaultPlane :: ValueMap
defaultPlane = fromFoldable [
  Tuple "colour" (ValueInt 0x888888),
  Tuple "shadows" (ValueBoolean true)
  ]
  
defaultBox :: ValueMap
defaultBox = fromFoldable [
  Tuple "colour" (ValueInt 0x888888),
  Tuple "shadows" (ValueBoolean true),
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


defaultCamera :: ValueMap
defaultCamera = fromFoldable [
  Tuple "x" (ValueNumber 0.0),
  Tuple "y" (ValueNumber 1.0),
  Tuple "z" (ValueNumber 10.0),
  Tuple "rx" (ValueNumber 0.0),
  Tuple "ry" (ValueNumber 0.0),
  Tuple "rz" (ValueNumber 0.0)
  ]

defaultClear :: ValueMap
defaultClear = fromFoldable [
  Tuple "colour" (ValueInt 0x000000),
  Tuple "alpha" (ValueNumber 1.0)
  ]
