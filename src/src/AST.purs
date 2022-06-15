module AST (
  Program(..),
  defaultProgram,
  Statement(..),
  Dancer(..),
  defaultDancer,
  Ethereal(..),
  defaultEthereal,
  Camera(..),
  Vec3(..)
  ) where

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
  Camera (List Camera)

instance Show Statement where
  show (Dancer x) = show x
  show (Camera x) = show x

type Dancer = {
  url :: String,
  animation :: Int,
  dur :: Number,
  pos :: Vec3,
  rot :: Vec3,
  scale :: Vec3
  }

defaultDancer :: Dancer
defaultDancer = { url: "raccoon.glb", animation: 0, dur: 1.0, pos: origin, rot: origin, scale: defaultScale }

-- and for now Ethereals are just polarGridHelpers...

data Ethereal = PolarGridHelper {
  radius :: Number, -- three.js default is 10, must be positive
  radials :: Int, -- three.js default is 16, must be positive
  circles :: Int, -- three.js default is 8, must be positive
  divisions :: Int, -- three.js default is 64, must be 3 or greater
  pos :: Vec3
  }

defaultEthereal :: Ethereal
defaultEthereal = PolarGridHelper {
  radius: 10.0,
  radials: 16,
  circles: 8,
  divisions: 64,
  pos: origin
  }

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
