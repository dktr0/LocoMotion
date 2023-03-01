module Dancer (newDancer,updateDancer,removeDancer) where

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
import Model

newDancer :: R Dancer
newDancer = liftEffect $ do
  url <- new ""
  model <- new Nothing
  pure { url, model }

updateDancer :: ValueMap -> Dancer -> R Dancer
updateDancer vm x = do
  y <- updateModel vm x
  whenMaybeRef y.model $ \m -> do
    updateTransforms vm m.scene
    updateAnimation vm y
  pure x

removeDancer :: Dancer -> R Unit
removeDancer d = do
   env <- ask
   liftEffect $ whenMaybeRef d.model $ \m -> Three.removeObject3D env.scene m.scene



updateModel :: ValueMap -> Dancer -> R Dancer
updateModel vm d = do
  let urlProg = lookupString "raccoon.glb" "url" vm
  urlState <- liftEffect $ read d.url
  when (urlProg /= urlState) $ do
    removeDancer d
    loadModel urlProg d
  pure d


updateAnimation :: ValueMap -> Dancer -> R Unit
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


loadModel :: String -> Dancer -> R Unit
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
