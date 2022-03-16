module AST (
  Program(..),
  defaultProgram,
  Statement(..),
  Element(..),
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
import Data.List.NonEmpty
import Data.Either
import Data.Maybe (Maybe)
import Data.Number (fromString)
import Control.Bind

-- Program-s are semi-colon separated lists of Statement-s

type Program = List Statement

defaultProgram :: Program
defaultProgram = Nil

-- Statement-s describe an Element in the scene
-- or they change some other parameter of the rendering (eg. camera settings)

data Statement =
  Element Element |
  CameraChange (Camera -> Camera)

-- Element-s can be Dancer-s (which can be animated, have physics, etc)
-- or Ethereal-s (which are displayed but do not have physics [maybe they might have animation, though?])

data Element =
  Dancer Dancer |
  Ethereal Ethereal

type Dancer = {
  url :: String,
  animation :: Int,
  pos :: Vec3,
  rot :: Vec3
  }

defaultDancer :: Dancer
defaultDancer = { url: "model.glb", animation: 4, pos: origin, rot: origin }

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

type Camera = {
  pos :: Vec3,
  rot :: Vec3
  }

-- normally I wouldn't use x,y,z as the name of a record field
-- but perhaps this is more okay with purescript? Let's see...

type Vec3 = {
  x :: Number,
  y :: Number,
  z :: Number
  }

origin :: Vec3
origin = {x:0.0, y:0.0, z:0.0}
