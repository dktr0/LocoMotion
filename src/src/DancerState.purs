module DancerState (
  runDancerWithState,
  removeDancer
  )
  where

import Prelude
import Data.Number (pi)
import Data.Array
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Ref (Ref, new, read, write)
import Effect.Console (log)
import Effect.Class (liftEffect)
import ThreeJS as Three
import Data.Rational
import Data.Ratio
import Data.Traversable (traverse,traverse_)
import Data.FoldableWithIndex (traverseWithIndex_)
import Data.Map (Map)
import Control.Monad.Reader.Trans (ask)

import URL
import Value
import MaybeRef
import R


runDancerWithState :: ValueMap -> Maybe DancerState -> R DancerState
runDancerWithState vm maybeDancerState = do
  s <- loadModelIfNecessary vm maybeDancerState
  whenMaybeRef s.model $ \m -> do
    updateTransforms vm m.scene
    updateAnimation vm s
  pure s


loadModelIfNecessary :: ValueMap -> Maybe DancerState -> R DancerState
loadModelIfNecessary vm Nothing = do
  let urlProg = lookupString "raccoon.glb" "url" vm
  url <- liftEffect $ new urlProg
  model <- liftEffect $ new Nothing
  let s = { url, model }
  loadModel urlProg s
  pure s
loadModelIfNecessary vm (Just s) = do
  let urlProg = lookupString "raccoon.glb" "url" vm
  urlState <- liftEffect $ read s.url
  when (urlProg /= urlState) $ do
    removeDancer s
    loadModel urlProg s
  pure s


updateAnimation :: ValueMap -> DancerState -> R Unit
updateAnimation valueMap s = do
  env <- ask
  liftEffect $ whenMaybeRef s.model $ \m -> do
    prevMixerState <- read m.mixerState
    let newMixerState = valueToMixerState m $ lookupValue (ValueInt 0) "animation" valueMap
    prevDurState <- read m.durState
    let dur = lookupNumber 1.0 "dur" valueMap * env.cycleDur
    when (prevMixerState /= newMixerState || prevDurState /= dur) $ do
      -- log $ "prevMixerState /= newMixerState, dur = " <> show dur
      -- log $ show prevMixerState <> " ... " <> show newMixerState
      traverseWithIndex_ (updateAnimationAction m dur) newMixerState
      write newMixerState m.mixerState
      write dur m.durState
    Three.updateAnimationMixer m.mixer env.delta


updateAnimationAction :: Model -> Number -> Int -> Number -> Effect Unit
updateAnimationAction m dur i weight = do
  case m.actions!!i of
    Just a -> do
      case weight of
        0.0 -> do
          -- log $ "stopping action" <> show i
          Three.stop a
        _ -> do
          -- log $ "updating action " <> show i <> " with weight " <> show weight <> " and duration " <> show dur
          Three.setEffectiveTimeScale a 1.0
          Three.playAnything a
          Three.setDuration a dur
    Nothing -> log "strange error in LocoMotion: updateAnimationAction, should not be possible"


loadModel :: String -> DancerState -> R Unit
loadModel url s = do
  env <- ask
  liftEffect $ write url s.url
  let url' = resolveURL url
  _ <- liftEffect $ Three.loadGLTF_DRACO "https://dktr0.github.io/LocoMotion/threejs/" url' $ \gltf -> do
    log $ "model " <> url' <> " loaded with " <> show (length gltf.animations) <> " animations"
    traverseWithIndex_ logAnimation gltf.animations
    Three.addAnything env.scene gltf.scene
    m <- gltfToModel gltf
    write (Just m) s.model
  pure unit

logAnimation :: Int -> Three.AnimationClip -> Effect Unit
logAnimation i x = log $ " " <> show i <> ": " <> show x.name


gltfToModel :: Three.GLTF -> Effect Model
gltfToModel gltf = do
  let clipNames = map (_.name) gltf.animations
  mixer <- Three.newAnimationMixer gltf.scene -- make an animation mixer
  actions <- traverse (Three.clipAction mixer) gltf.animations -- convert all animations to AnimationActions connected to the animation mixer
  mixerState <- new []
  durState <- new (-999.0)
  pure { scene: gltf.scene, clips: gltf.animations, clipNames, mixer, actions, mixerState, durState }


removeDancer :: DancerState -> R Unit
removeDancer s = do
   env <- ask
   liftEffect $ whenMaybeRef s.model $ \m -> Three.removeObject3D env.scene m.scene
