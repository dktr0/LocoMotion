module DancerState (
  DancerState(..),
  Model(..),
  MixerState(..),
  runDancerWithState,
  removeDancer
  )
  where

import Prelude
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
import Data.Map (Map)
import Control.Monad.Reader.Trans (ask)

import URL
import Value
import MaybeRef
import R

type DancerState =
  {
  url :: Ref String,
  model :: MaybeRef Model,
  prevAnimationIndex :: Ref Int, -- OBSOLETE: will be removed when MixerState refactor complete
  prevAnimationAction :: MaybeRef Three.AnimationAction -- OBSOLETE: will be removed when MixerState refactor complete
  }

type Model = {
  scene :: Three.Scene,
  clips :: Array Three.AnimationClip,
  mixer :: Three.AnimationMixer,
  actions :: Array Three.AnimationAction,
  mixerState :: Ref MixerState
  }

-- the state of the animation system is represented as an array of floating point weights
-- this is cached in the Model after update, so that in succeeding frames, the calculated
-- MixerState (calculated from AnimationExpr + "environment") can be compared to determine
-- if any update to the underlying AnimationMixer is required or not.

type MixerState = Array Number

{-
animationExprToMixerState :: AnimationExpr -> Array Three.AnimationAction -> Effect MixerState
animationExprToMixerState (AnimationIndexInt i) actions = pure $ ... an array where weight of i is 1.0 and all other weights is 0
animationExprToMixerState (AnimationIndexString s) actions = do
  i <- ...figure out the int index of the animation indexed by s...
  pure $ exprToMixerState (AnimationIndexInt i) actions
animationExprToAnimationMixerState (AnimationMix xs) actions = do
  ...xs :: (List (Tuple AnimationExpr Variable)) -- example: ["headroll" 0.5, "legroll" (osc 0.5 * 0.2)]
-}


runDancerWithState :: ValueMap -> Maybe DancerState -> R DancerState
runDancerWithState vm maybeDancerState = do
  s <- loadModelIfNecessary vm maybeDancerState
  updateTransforms vm s
  updateAnimation vm s
  pure s


loadModelIfNecessary :: ValueMap -> Maybe DancerState -> R DancerState
loadModelIfNecessary vm Nothing = do
  let urlProg = lookupString "raccoon.glb" "url" vm
  url <- liftEffect $ new urlProg
  model <- liftEffect $ new Nothing
  prevAnimationIndex <- liftEffect $ new (-9999)
  prevAnimationAction <- liftEffect $ new Nothing
  let s = { url, model, prevAnimationIndex, prevAnimationAction }
  loadModel urlProg s
  pure s
loadModelIfNecessary vm (Just s) = do
  let urlProg = lookupString "raccoon.glb" "url" vm
  urlState <- liftEffect $ read s.url
  when (urlProg /= urlState) $ do
    removeDancer s
    loadModel urlProg s
  pure s


updateTransforms :: ValueMap -> DancerState -> R Unit
updateTransforms valueMap s = do
  x <- realizeNumber "x" 0.0 valueMap
  y <- realizeNumber "y" 0.0 valueMap
  z <- realizeNumber "z" 0.0 valueMap
  rx <- realizeNumber "rx" 0.0 valueMap
  ry <- realizeNumber "ry" 0.0 valueMap
  rz <- realizeNumber "rz" 0.0 valueMap
  sx <- realizeNumber "sx" 1.0 valueMap
  sy <- realizeNumber "sy" 1.0 valueMap
  sz <- realizeNumber "sz" 1.0 valueMap
  size <- realizeNumber "size" 1.0 valueMap
  liftEffect $ whenMaybeRef s.model $ \m -> do
    Three.setPositionOfAnything m.scene x y z
    Three.setRotationOfAnything m.scene rx ry rz
    Three.setScaleOfAnything m.scene (sx*size) (sy*size) (sz*size)


updateAnimation :: ValueMap -> DancerState -> R Unit
updateAnimation valueMap s = do
  env <- ask
  liftEffect $ whenMaybeRef s.model $ \m -> do
    playAnimation s $ lookupInt 0 "animation" valueMap
    let dur = lookupNumber 1.0 "dur" valueMap
    updateAnimationDuration s $ dur * env.cycleDur
    Three.updateAnimationMixer m.mixer env.delta


loadModel :: String -> DancerState -> R Unit
loadModel url s = do
  env <- ask
  liftEffect $ write url s.url
  let url' = resolveURL url
  _ <- liftEffect $ Three.loadGLTF_DRACO "https://dktr0.github.io/LocoMotion/threejs/" url' $ \gltf -> do
    log $ "model " <> url' <> " loaded with " <> show (length gltf.animations) <> " animations"
    Three.addAnything env.scene gltf.scene
    m <- gltfToModel gltf
    write (Just m) s.model
    animIndex <- read s.prevAnimationIndex
    case animIndex of
      (-9999) -> playAnimation s 0
      x -> playAnimation s x
  pure unit

gltfToModel :: Three.GLTF -> Effect Model
gltfToModel gltf = do
  mixer <- Three.newAnimationMixer gltf.scene -- make an animation mixer
  actions <- traverse (Three.clipAction mixer) gltf.animations -- convert all animations to AnimationActions connected to the animation mixer
  mixerState <- new []
  pure { scene: gltf.scene, clips: gltf.animations, mixer, actions, mixerState }


removeDancer :: DancerState -> R Unit
removeDancer s = do
   env <- ask
   liftEffect $ whenMaybeRef s.model $ \m -> Three.removeObject3D env.scene m.scene


playAnimation :: DancerState -> Int -> Effect Unit
playAnimation s n = whenMaybeRef s.model $ \m -> do
  let nActions = length m.actions
  prevN <- read s.prevAnimationIndex
  when ((prevN /= n) && (nActions > 0)) $ do
    let n' = mod n nActions
    case m.actions!!n' of
      Just newAction -> do
        z <- read s.prevAnimationAction
        case z of
          Just oldAction -> do
            Three.stop oldAction
            Three.playAnything newAction
          Nothing -> do
            Three.setEffectiveTimeScale newAction 1.0
            Three.playAnything newAction
            write (Just newAction) s.prevAnimationAction
      Nothing -> log "strange error in LocoMotion: DancerState: playAnimation"
    write n s.prevAnimationIndex


updateAnimationDuration :: DancerState -> Number -> Effect Unit
updateAnimationDuration s dur = whenMaybeRef s.prevAnimationAction $ \a -> Three.setDuration a dur
