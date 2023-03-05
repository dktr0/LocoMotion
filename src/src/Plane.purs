module Plane (newPlane,updatePlane,removePlane) where

import Prelude
import ThreeJS as Three
import Effect (Effect)
import Effect.Console (log)
import Effect.Class (liftEffect)
import Data.Map (Map)
import Control.Monad.Reader.Trans (ask)
import Data.Maybe (Maybe(..))

import Value
import R

newPlane :: R Plane
newPlane = do
  geometry <- liftEffect $ Three.newPlaneGeometry 1.0 1.0 1 1
  material <- liftEffect $ Three.newMeshPhongMaterial { depthWrite: false }
  mesh <- liftEffect $ Three.newMesh geometry material
  env <- ask
  liftEffect $ Three.addAnything env.scene mesh
  pure { mesh, material }

updatePlane :: ValueMap -> Plane -> R Plane
updatePlane vm fs = do
  let colour = lookupInt 0x888888 "colour" vm
  let shadows = lookupBoolean true "shadows" vm
  liftEffect $ Three.setColorInt fs.material colour
  liftEffect $ Three.setReceiveShadow fs.mesh shadows
  updatePosition vm fs.mesh
  updateScale vm fs.mesh
  updateRotation vm fs.mesh
  pure fs

removePlane :: Plane -> R Unit
removePlane fState = liftEffect $ Three.removeFromParent fState.mesh
