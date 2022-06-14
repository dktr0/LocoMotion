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

-- Statement-s describe an Element in the scene
-- or they change some other parameter of the rendering (eg. camera settings)

data Statement =
  Element Element |
  CameraChange (Camera -> Camera)

instance Show Statement where
  show (Element x) = show x
  show (CameraChange _) = "CameraChange _"

-- Element-s can be Dancer-s (which can be animated, have physics, etc)
-- or Ethereal-s (which are displayed but do not have physics [maybe they might have animation, though?])

data Element =
  Dancer Dancer |
  Ethereal Ethereal

instance Show Element where
  show (Dancer x) = show x
  show (Ethereal x) = "um an ethereal."

type Dancer = {
  url :: String,
  animation :: Int,
  pos :: Vec3,
  rot :: Vec3,
  scale :: Vec3
  }

defaultDancer :: Dancer
defaultDancer = { url: "WomanInTheSea.glb", animation: 4, pos: origin, rot: origin, scale: defaultScale }

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
  x :: Variable,
  y :: Variable,
  z :: Variable
  }

origin :: Vec3
origin = { x: Constant 0.0, y: Constant 0.0, z: Constant 0.0 }

defaultScale :: Vec3
defaultScale = { x: Constant 1.0, y: Constant 1.0, z: Constant 1.0 }
