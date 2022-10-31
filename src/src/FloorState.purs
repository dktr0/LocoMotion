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
import Transformer
import ValueMap


type FloorState = {
  mesh :: Three.Mesh,
  material :: Three.MeshPhongMaterial
  }

newFloorState :: Three.Scene -> Number -> Transformer -> Effect FloorState
newFloorState scene nCycles t = do
  log "adding floor"
  let valueMap = realizeTransformer nCycles t
  let colour = lookupInt 0x888888 "colour" valueMap
  let shadows = lookupBoolean true "shadows" valueMap
  geometry <- Three.newPlaneGeometry 100.0 100.0 1 1
  material <- Three.newMeshPhongMaterial { color: colour, depthWrite: false }
  mesh <- Three.newMesh geometry material
  Three.setRotationX mesh 4.71238898 -- 3/2 x PI
  Three.setReceiveShadow mesh shadows
  Three.addAnything scene mesh
  pure { mesh, material }

runFloorState :: Number -> Transformer -> FloorState -> Effect Unit
runFloorState nCycles t fState = do
  let valueMap = realizeTransformer nCycles t
  let colour = lookupInt 0x888888 "colour" valueMap
  let shadows = lookupBoolean true "shadows" valueMap
  Three.setColorInt fState.material colour
  Three.setReceiveShadow fState.mesh shadows

removeFloorState :: FloorState -> Effect Unit
removeFloorState fState = do
  log "removing floor"
  Three.removeFromParent fState.mesh
