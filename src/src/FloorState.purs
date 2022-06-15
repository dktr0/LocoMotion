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

import AST


type FloorState = {
  mesh :: Three.Mesh,
  material :: Three.MeshPhongMaterial
  }

newFloorState :: Three.Scene -> Floor -> Effect FloorState
newFloorState scene f = do
  log "adding floor"
  geometry <- Three.newPlaneGeometry 100.0 100.0 1 1
  material <- Three.newMeshPhongMaterial { color: f.colour, depthWrite: false }
  mesh <- Three.newMesh geometry material
  Three.setRotationX mesh 4.71238898 -- 3/2 x PI
  Three.setReceiveShadow mesh f.shadows
  Three.addAnything scene mesh
  pure { mesh, material }


runFloorState :: Floor -> FloorState -> Effect Unit
runFloorState f fState = do
  Three.setColorInt fState.material f.colour
  Three.setReceiveShadow fState.mesh f.shadows

removeFloorState :: FloorState -> Effect Unit
removeFloorState fState = do
  log "removing floor"
  Three.removeFromParent fState.mesh
