module FloorState (
  FloorState,
  newFloorState,
  runFloorState,
  removeFloorState
  ) where

import Prelude
import ThreeJS as Three
import Effect (Effect)
import Effect.Console (log)
import Effect.Class (liftEffect)
import Data.Map (Map)
import Control.Monad.Reader.Trans (ask)

import Value
import R

type FloorState = {
  mesh :: Three.Mesh,
  material :: Three.MeshPhongMaterial
  }

newFloorState :: ValueMap -> R FloorState
newFloorState valueMap = do
  env <- ask
  let colour = lookupInt 0x888888 "colour" valueMap
  let shadows = lookupBoolean true "shadows" valueMap
  geometry <- liftEffect $ Three.newPlaneGeometry 100.0 100.0 1 1
  material <- liftEffect $ Three.newMeshPhongMaterial { color: colour, depthWrite: false }
  mesh <- liftEffect $ Three.newMesh geometry material
  liftEffect $ Three.setRotationX mesh 4.71238898 -- 3/2 x PI
  liftEffect $ Three.setReceiveShadow mesh shadows
  liftEffect $ Three.addAnything env.scene mesh
  pure { mesh, material }

runFloorState :: ValueMap -> FloorState -> R Unit
runFloorState valueMap fState = do
  let colour = lookupInt 0x888888 "colour" valueMap
  let shadows = lookupBoolean true "shadows" valueMap
  liftEffect $ Three.setColorInt fState.material colour
  liftEffect $ Three.setReceiveShadow fState.mesh shadows

removeFloorState :: FloorState -> R Unit
removeFloorState fState = liftEffect $ Three.removeFromParent fState.mesh
