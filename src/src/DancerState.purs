module DancerState where

import Prelude
import Data.Array
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Ref (Ref, new, read, write)
import Effect.Console (log)
import Graphics.Three.Scene (Scene) as Three
import ThreeJS as Three
import AST (Dancer)
import Variable
import URL

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
  let url' = resolveURL d.url
  Three.loadGLTF url' $ \gltf -> do
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

runDancer :: Number -> Dancer -> DancerState -> Effect DancerState
runDancer t d dState = do
  ms <- read dState.gltfScene
  case ms of
    Just s -> do
      let x'  = sampleVariable t d.pos.x
      let y'  = sampleVariable t d.pos.y
      let z'  = sampleVariable t d.pos.z
      let rx'  = sampleVariable t d.rot.x
      let ry'  = sampleVariable t d.rot.y
      let rz'  = sampleVariable t d.rot.z
      let sx'  = sampleVariable t d.scale.x
      let sy'  = sampleVariable t d.scale.y
      let sz'  = sampleVariable t d.scale.z
      -- log $ show t <> " " <> show x'
      Three.setPositionOfAnything s x' y' z'
      Three.setRotationOfAnything s rx' ry' rz'
      Three.setScaleOfAnything s sx' sy' sz'
      am0 <- read dState.animationMixer
      case am0 of
        Just am -> Three.updateAnimationMixer am 0.016666
        Nothing -> pure unit
    Nothing -> pure unit
  pure dState
