module Floor (
  runFloorWithState,
  removeFloor
  ) where

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


type FloorState = {
  mesh :: Three.Mesh,
  material :: Three.MeshPhongMaterial
  }
  
runFloorWithState :: ValueMap -> Maybe FloorState -> R FloorState
runFloorWithState vm Nothing = do
  x <- newFloor
  runFloor vm x
runFloorWithState vm (Just x) = runFloor vm x

newFloor :: R FloorState
newFloor = do
  geometry <- liftEffect $ Three.newPlaneGeometry 100.0 100.0 1 1
  material <- liftEffect $ Three.newMeshPhongMaterial { depthWrite: false }
  mesh <- liftEffect $ Three.newMesh geometry material
  liftEffect $ Three.setRotationX mesh 4.71238898 -- 3/2 x PI
  env <- ask
  liftEffect $ Three.addAnything env.scene mesh
  pure { mesh, material }

runFloor :: ValueMap -> FloorState -> R FloorState
runFloor vm fs = do
  let colour = lookupInt 0x888888 "colour" vm
  let shadows = lookupBoolean true "shadows" vm
  liftEffect $ Three.setColorInt fs.material colour
  liftEffect $ Three.setReceiveShadow fs.mesh shadows
  pure fs

removeFloor :: FloorState -> R Unit
removeFloor fState = liftEffect $ Three.removeFromParent fState.mesh
