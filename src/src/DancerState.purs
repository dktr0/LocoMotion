module DancerState (
  DancerState(..),
  MaybeRef(..),
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
import ThreeJS as Three
import Data.Rational
import Data.Ratio

import AST (Dancer)
import Variable
import URL

type MaybeRef a = Ref (Maybe a)

type DancerState =
  {
  url :: Ref String,
  theDancer :: MaybeRef Three.Scene,
  animations :: MaybeRef (Array Three.AnimationClip),
  animationMixer :: MaybeRef Three.AnimationMixer
  }


newDancerState :: String -> Effect DancerState
newDancerState x = do
  url <- new x
  theDancer <- new Nothing
  animations <- new Nothing
  animationMixer <- new Nothing
  pure { url, theDancer, animations, animationMixer }


runDancerWithState :: Three.Scene -> Number -> Dancer -> Maybe DancerState -> Effect DancerState
runDancerWithState theScene nCycles d maybeDancerState = do
  dState <- case maybeDancerState of
    Nothing -> addDancer theScene d
    Just x -> do
      updateModelIfNecessary theScene d x
      pure x
  ms <- read dState.theDancer
  case ms of
    Just s -> do
      let x'  = sampleVariable nCycles d.pos.x
      let y'  = sampleVariable nCycles d.pos.y
      let z'  = sampleVariable nCycles d.pos.z
      let rx'  = sampleVariable nCycles d.rot.x
      let ry'  = sampleVariable nCycles d.rot.y
      let rz'  = sampleVariable nCycles d.rot.z
      let sx'  = sampleVariable nCycles d.scale.x
      let sy'  = sampleVariable nCycles d.scale.y
      let sz'  = sampleVariable nCycles d.scale.z
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


addDancer :: Three.Scene -> Dancer -> Effect DancerState
addDancer theScene d = do
  dState <- newDancerState d.url
  loadModel theScene d.url dState
  pure dState


updateModelIfNecessary :: Three.Scene -> Dancer -> DancerState -> Effect Unit
updateModelIfNecessary theScene d dState = do
  let urlProg = d.url
  urlState <- read dState.url
  when (urlProg /= urlState) $ do
    removeDancer theScene dState
    loadModel theScene d.url dState


loadModel :: Three.Scene -> String -> DancerState -> Effect Unit
loadModel theScene url dState = do
  write url dState.url
  let url' = resolveURL url
  _ <- Three.loadGLTF_DRACO "https://dktr0.github.io/LocoMotion/threejs/" url' $ \gltf -> do
    log $ "model loaded with " <> show (length gltf.animations) <> " animations"
    Three.addAnything theScene gltf.scene
    write (Just gltf.scene) dState.theDancer
    write (Just gltf.animations) dState.animations
    animMixer <- Three.newAnimationMixer gltf.scene
    write (Just animMixer) dState.animationMixer
    case gltf.animations!!0 of
      Just a -> do
        defaultAction <- Three.clipAction animMixer a
        Three.setEffectiveTimeScale defaultAction 1.0
        Three.playAnything defaultAction
      Nothing -> pure unit
  pure unit


removeDancer :: Three.Scene -> DancerState -> Effect Unit
removeDancer sc d = do
  x <- read d.theDancer
  case x of
    Just y -> Three.removeObject3D sc y
    Nothing -> pure unit
