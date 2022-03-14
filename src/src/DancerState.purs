module DancerState where

import Prelude
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Ref (Ref, new, read, write)
import Effect.Console (log)
import Graphics.Three.Scene (Scene) as Three
import Graphics.Three.Object3D (setPosition) as Three
import ThreeJS as Three
import AST (Dancer)

type DancerState =
  {
  gltfScene :: Ref (Maybe Three.Scene)
  }

addDancer :: Three.Scene -> Dancer -> Effect DancerState
addDancer theScene d = do
  gltfScene <- new Nothing
  Three.loadGLTF d.url $ \gltf -> do
    log "model loaded!"
    Three.addAnythingToScene theScene gltf.scene
    write (Just gltf.scene) gltfScene
  pure { gltfScene }

runDancer :: Dancer -> DancerState -> Effect DancerState
runDancer d dState = do
  ms <- read dState.gltfScene
  case ms of
    Just s -> do
      Three.setPositionOfAnything s d.pos.x d.pos.y d.pos.z
    Nothing -> pure unit
  pure dState
