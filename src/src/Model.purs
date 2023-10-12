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

valueToMixerState :: Model -> Value -> Array Number
valueToMixerState m (ValueInt i) = intToMixerState (length m.actions) i
valueToMixerState m (ValueNumber n) = intToMixerState (length m.actions) (floor n) -- later: could be a crossfade
valueToMixerState m (ValueString v) = fromMaybe allZeros $ updateAt n' 1.0 allZeros
  where
    n' = fromMaybe 0 $ elemIndex v m.clipNames
    allZeros = replicate (length m.actions) 0.0
valueToMixerState _ _ = []

intToMixerState :: Int -> Int -> Array Number
intToMixerState nAnimations n = fromMaybe allZeros $ updateAt n' 1.0 allZeros
  where
    n' = mod n nAnimations
    allZeros = replicate nAnimations 0.0
    
deleteModel :: Model -> Effect Unit
deleteModel m = Three.removeFromParent m.scene
