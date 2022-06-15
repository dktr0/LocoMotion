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

import Variable

-- Program-s consist of some number of semi-colon separated lists of Statement-s

type Program = Map Int Statement

defaultProgram :: Program
defaultProgram = empty

-- Statement-s describe an Element in the scene (eg. a Dancer)
-- or they change some other parameter of the rendering (eg. camera settings)

data Statement =
  Dancer Dancer |
  Floor Floor |
  Camera (List Camera)

instance Show Statement where
  show (Dancer x) = show x
  show (Floor x) = show x
  show (Camera x) = show x

type Dancer = {
  url :: String,
  animation :: Int,
  dur :: Variable,
  pos :: Vec3,
  rot :: Vec3,
  scale :: Vec3
  }

defaultDancer :: Dancer
defaultDancer = { url: "raccoon.glb", animation: 0, dur: Constant 1.0, pos: origin, rot: origin, scale: defaultScale }

type Floor = {
  colour :: Int,
  shadows :: Boolean
  }

defaultFloor :: Floor
defaultFloor = { colour: 0x101010, shadows: true }

data Camera =
  CameraX Variable |
  CameraY Variable |
  CameraZ Variable |
  CameraRotX Variable |
  CameraRotY Variable |
  CameraRotZ Variable

instance Show Camera where
  show (CameraX v) = "CameraX " <> show v
  show (CameraY v) = "CameraY " <> show v
  show (CameraZ v) = "CameraZ " <> show v
  show (CameraRotX v) = "CameraRotX " <> show v
  show (CameraRotY v) = "CameraRotY " <> show v
  show (CameraRotZ v) = "CameraRotZ " <> show v

type Vec3 = {
  x :: Variable,
  y :: Variable,
  z :: Variable
  }

origin :: Vec3
origin = { x: Constant 0.0, y: Constant 0.0, z: Constant 0.0 }

defaultScale :: Vec3
defaultScale = { x: Constant 1.0, y: Constant 1.0, z: Constant 1.0 }
