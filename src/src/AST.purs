module AST where

import Prelude
import Data.Identity
import Data.List
import Data.Map
import Data.List.NonEmpty
import Data.Either
import Data.Maybe (Maybe)
import Data.Number (fromString)
import Control.Bind

import AnimationExpr
import Transformer

-- Program-s consist of some number of semi-colon separated lists of Statement-s

type Program = Map Int Statement

defaultProgram :: Program
defaultProgram = empty

-- Statement-s describe an Element in the scene (eg. a Dancer)
-- or they change some other parameter of the rendering (eg. camera settings)

data Statement =
  Dancer Transformer |
  Floor Transformer |
  Camera Transformer |
  Assignment String ValueExpr |
  EmptyStatement

instance Show Statement where
  show (Dancer x) = "Dancer " <> show x
  show (Floor x) = "Floor " <> show x
  show (Camera x) = "Camera " <> show x
  show (Assignment k v) = "Assignment " <> show k <> " " <> show v
  show EmptyStatement = "EmptyStatement"

isDancer :: Statement -> Boolean
isDancer (Dancer _) = true
isDancer _ = false

isFloor :: Statement -> Boolean
isFloor (Floor _) = true
isFloor _ = false
