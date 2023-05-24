module Dancer (newDancer,updateDancer,removeDancer) where

import Prelude
import Data.Number (pi,floor)
import Data.Int (toNumber,round)
import Data.Array
import Data.Maybe (Maybe(..),fromMaybe)
import Effect (Effect)
import Effect.Ref (Ref, new, read, write)
import Effect.Console (log)
import Effect.Class (liftEffect)
import ThreeJS as Three
import Data.Rational
import Data.Ratio
import Data.Traversable (traverse,traverse_)
import Data.FoldableWithIndex (traverseWithIndex_)
import Data.Map (Map,lookup)
import Control.Monad.Reader.Trans (ask)
import Data.Newtype (unwrap)
import Data.DateTime.Instant (unInstant,fromDateTime)

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

updateDancer :: Int -> ValueMap -> Dancer -> R Dancer
updateDancer zone vm x = do
  y <- updateModel zone vm x
  whenMaybeRef y.model $ \m -> do
    updatePosition vm m.scene
    updateScale vm m.scene
    updateRotation vm m.scene
    updateAnimation zone vm y
  pure x

removeDancer :: Dancer -> R Unit
removeDancer d = do
   env <- ask
   liftEffect $ whenMaybeRef d.model $ \m -> Three.removeObject3D env.scene m.scene



updateModel :: Int -> ValueMap -> Dancer -> R Dancer
updateModel zone vm d = do
  urlProg <- calculateModelURL zone vm
  urlState <- liftEffect $ read d.url
  when (urlProg /= urlState) $ do
    removeDancer d
    loadModel urlProg d
  pure d
  

calculateModelURL :: Int -> ValueMap -> R String
calculateModelURL zone vm = 
  case lookupString "" "url" vm of 
    "" -> randomModel zone 0
    x -> pure x
    

randomModel :: Int -> Int -> R String
randomModel zone increment = do
  env <- ask 
  let secs = (unwrap $ unInstant $ fromDateTime $ env.tempo.time) / 1000.0
  let nModels = length models
  let nBase = round $ (secs - floor secs) * toNumber nModels
  let n = mod (nBase + zone + increment) nModels
  pure $ fromMaybe "raccoon.glb" $ index models n


models :: Array String
models = [
  "cactus.glb","Daffy.glb","Lily.glb",
  "NatureGirl.glb","StoneFigure.glb","Willy.glb","Woman-NLA.glb",
  "ant.glb","Branch.glb","crackman.glb","Diver.glb","fossegrim.glb",
  "leafy.glb","Oak.glb","raccoon.glb","wireman.glb"
  ]


updateAnimation :: Int -> ValueMap -> Dancer -> R Unit
updateAnimation zone vm s = whenMaybeRef s.model $ \m -> do
    env <- ask
    prevMixerState <- liftEffect $ read m.mixerState
    let nAnims = length m.actions
    animationValue <- calculateAnimation zone nAnims vm
    let newMixerState = valueToMixerState m animationValue
    prevDurState <- liftEffect $ read m.durState
    let dur = lookupNumber 1.0 "dur" vm * env.cycleDur
    when (prevMixerState /= newMixerState || prevDurState /= dur) $ liftEffect $ do
      -- log $ "prevMixerState /= newMixerState, dur = " <> show dur
      -- log $ show prevMixerState <> " ... " <> show newMixerState
      traverseWithIndex_ (updateAnimationAction m dur) newMixerState
      write newMixerState m.mixerState
      write dur m.durState
    liftEffect $ Three.updateAnimationMixer m.mixer env.delta

calculateAnimation :: Int -> Int -> ValueMap -> R Value
calculateAnimation zone nAnims vm = 
  case lookup "animation" vm of
    Nothing -> randomAnimation zone 0 nAnims
    Just v -> pure v

randomAnimation :: Int -> Int -> Int -> R Value
randomAnimation zone increment nAnims = do
  env <- ask 
  let nModels = length models
  let secs = (unwrap $ unInstant $ fromDateTime $ env.tempo.time) / (1000.0 * toNumber nModels)
  let nBase = round $ (secs - floor secs) * toNumber nAnims
  let n = mod (nBase + zone + increment) nAnims
  pure $ ValueInt n
  

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
