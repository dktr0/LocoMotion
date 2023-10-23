module Model where

import Prelude
import Effect (Effect)
import Effect.Ref (Ref, new)
import Data.Array (elemIndex, length, replicate, updateAt)
import Data.Maybe (fromMaybe)
import ThreeJS as Three
import Data.Traversable (traverse)
import Data.Int (floor)

import Value

gltfToModel :: Three.GLTF -> Effect Model
gltfToModel gltf = do
  let clipNames = map (_.name) gltf.animations
  mixer <- Three.newAnimationMixer gltf.scene -- make an animation mixer
  actions <- traverse (Three.clipAction mixer) gltf.animations -- convert all animations to AnimationActions connected to the animation mixer
  mixerState <- new []
  durState <- new (-999.0)
  pure { scene: gltf.scene, clips: gltf.animations, clipNames, mixer, actions, mixerState, durState }

type Model = {
  scene :: Three.Scene,
  clips :: Array Three.AnimationClip,
  clipNames :: Array String,
  mixer :: Three.AnimationMixer,
  actions :: Array Three.AnimationAction,
  mixerState :: Ref MixerState,
  durState :: Ref Number
  }

type MixerState = Array Number

