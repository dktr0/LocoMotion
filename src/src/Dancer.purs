module Dancer (newDancer,updateDancer,removeDancer) where

import Prelude (Unit, bind, discard, mod, pure, show, unit, when, ($), (*), (+), (-), (/), (/=), (<>), (||))
import Data.Number (floor)
import Data.Int (toNumber,round,floor) as Int
import Data.Array
import Data.Maybe (Maybe(..),fromMaybe)
import Effect (Effect)
import Effect.Ref (new, read, write)
import Effect.Console (log)
import Effect.Class (liftEffect)
import ThreeJS as Three
import Data.FoldableWithIndex (traverseWithIndex_)
import Data.Map (Map,lookup)
import Control.Monad.Reader.Trans (ask)
import Data.Newtype (unwrap)
import Data.DateTime.Instant (unInstant,fromDateTime)

import URL (resolveURL)
import Value (Value(..), ValueMap, lookupNumber, lookupString) 
import MaybeRef (whenMaybeRef)
import R (Dancer, R, updatePosition, updateRotation, updateScale)
import Model (Model, gltfToModel)
import Variable (realizeVariable)

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
  let nBase = Int.round $ (secs - floor secs) * Int.toNumber nModels
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
    newMixerState <- valueToMixerState m animationValue
    prevDurState <- liftEffect $ read m.durState
    let dur = lookupNumber 1.0 "dur" vm * env.cycle
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
  let secs = (unwrap $ unInstant $ fromDateTime $ env.tempo.time) / (1000.0 * Int.toNumber nModels)
  let nBase = Int.round $ (secs - floor secs) * Int.toNumber nAnims
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
  let decoderPath = "https://raw.githubusercontent.com/mrdoob/three.js/dev/examples/jsm/libs/draco/"
  _ <- liftEffect $ Three.loadGLTF_DRACO decoderPath url' $ \gltf -> do
    log $ "model " <> url' <> " loaded with " <> show (length gltf.animations) <> " animations"
    traverseWithIndex_ logAnimation gltf.animations
    Three.addAnything env.scene gltf.scene
    m <- gltfToModel gltf
    write (Just m) s.model
  pure unit

logAnimation :: Int -> Three.AnimationClip -> Effect Unit
logAnimation i x = log $ " " <> show i <> ": " <> show x.name


valueToMixerState :: Model -> Value -> R (Array Number)
valueToMixerState m (ValueVariable v) = do
  rEnv <- ask
  let n = realizeVariable rEnv v
  pure $ intToMixerState (length m.actions) (Int.floor n)
valueToMixerState m (ValueInt i) = pure $ intToMixerState (length m.actions) i
valueToMixerState m (ValueNumber n) = pure $ intToMixerState (length m.actions) (Int.floor n) -- later: could be a crossfade
valueToMixerState m (ValueString v) = pure $ fromMaybe allZeros $ updateAt n' 1.0 allZeros
  where
    n' = fromMaybe 0 $ elemIndex v m.clipNames
    allZeros = replicate (length m.actions) 0.0
valueToMixerState _ _ = pure $ []

intToMixerState :: Int -> Int -> Array Number
intToMixerState nAnimations n = fromMaybe allZeros $ updateAt n' 1.0 allZeros
  where
    n' = mod n nAnimations
    allZeros = replicate nAnimations 0.0
    
