module DancerState where

import Prelude
import Data.Array
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Ref (Ref, new, read, write)
import Effect.Console (log)
import Graphics.Three.Scene (Scene) as Three
import Graphics.Three.Object3D (setPosition) as Three
import ThreeJS as Three
import AST (Dancer)

type MaybeRef a = Ref (Maybe a)

type DancerState =
  {
  gltfScene :: MaybeRef Three.Scene,
  animations :: MaybeRef (Array Three.AnimationClip),
  animationMixer :: MaybeRef Three.AnimationMixer
  }

addDancer :: Three.Scene -> Dancer -> Effect DancerState
addDancer theScene d = do
  gltfScene <- new Nothing
  animations <- new Nothing
  animationMixer <- new Nothing
  Three.loadGLTF d.url $ \gltf -> do
    log $ "model loaded with " <> show (length gltf.animations) <> " animations"
    Three.addAnythingToScene theScene gltf.scene
    write (Just gltf.scene) gltfScene
    write (Just gltf.animations) animations
    animMixer <- Three.newAnimationMixer gltf.scene
    write (Just animMixer) animationMixer
    case gltf.animations!!0 of
      Just a -> do
        log "playing default (first) animation"
        defaultAction <- Three.clipAction animMixer a
        Three.setEffectiveTimeScale defaultAction 1.0
        Three.play defaultAction
      Nothing -> pure unit
  pure { gltfScene, animations, animationMixer }

runDancer :: Dancer -> DancerState -> Effect DancerState
runDancer d dState = do
  ms <- read dState.gltfScene
  case ms of
    Just s -> do
      Three.setPositionOfAnything s d.pos.x d.pos.y d.pos.z
      am0 <- read dState.animationMixer
      case am0 of
        Just am -> Three.updateAnimationMixer am 0.016666
        Nothing -> pure unit
    Nothing -> pure unit
  pure dState
