module Floor (newFloor,updateFloor,removeFloor) where

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

newFloor :: R Floor
newFloor = do
  liftEffect $ log "newFloor"
  geometry <- liftEffect $ Three.newPlaneGeometry 100.0 100.0 1 1
  material <- liftEffect $ Three.newMeshPhongMaterial { depthWrite: false }
  mesh <- liftEffect $ Three.newMesh geometry material
  liftEffect $ Three.setRotationX mesh 4.71238898 -- 3/2 x PI
  env <- ask
  liftEffect $ Three.addAnything env.scene mesh
  pure { mesh, material }

updateFloor :: ValueMap -> Floor -> R Floor
updateFloor vm fs = do
  liftEffect $ log "updateFloor"
  let colour = lookupInt 0x888888 "colour" vm
  let shadows = lookupBoolean true "shadows" vm
  liftEffect $ Three.setColorInt fs.material colour
  liftEffect $ Three.setReceiveShadow fs.mesh shadows
  pure fs

removeFloor :: Floor -> R Unit
removeFloor fState = do
  liftEffect $ log "removeFloor"
  liftEffect $ Three.removeFromParent fState.mesh
