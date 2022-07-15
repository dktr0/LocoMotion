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
import Data.Traversable (traverse,traverse_)

import AnimationExpr
import AST (Dancer)
import Variable
import URL

type MaybeRef a = Ref (Maybe a)

type DancerState =
  {
  url :: Ref String,
  theDancer :: MaybeRef Three.Scene,
  animations :: MaybeRef (Array Three.AnimationClip),
  animationMixer :: MaybeRef Three.AnimationMixer,
  clipActions :: Ref (Array Three.AnimationAction),
  prevAnimationIndex :: Ref Int,
  prevAnimationAction :: MaybeRef Three.AnimationAction
  }


runDancerWithState :: Three.Scene -> Number -> Number -> Number -> Dancer -> Maybe DancerState -> Effect DancerState
runDancerWithState theScene cycleDur nCycles delta d maybeDancerState = do
  dState <- loadModelIfNecessary theScene d maybeDancerState
  playAnimation dState (animationExprToIntHack d.animation)
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
      updateAnimationDuration dState $ sampleVariable nCycles d.dur * cycleDur
      am0 <- read dState.animationMixer
      case am0 of
        Just am -> Three.updateAnimationMixer am delta
        Nothing -> pure unit
    Nothing -> pure unit
  pure dState


loadModelIfNecessary :: Three.Scene -> Dancer -> Maybe DancerState -> Effect DancerState
loadModelIfNecessary theScene d Nothing = do
  url <- new d.url
  theDancer <- new Nothing
  animations <- new Nothing
  animationMixer <- new Nothing
  clipActions <- new []
  prevAnimationIndex <- new (-9999)
  prevAnimationAction <- new Nothing
  let dState = { url, theDancer, animations, animationMixer, clipActions, prevAnimationIndex, prevAnimationAction }
  loadModel theScene d.url dState
  pure dState
loadModelIfNecessary theScene d (Just dState) = do
  let urlProg = d.url
  urlState <- read dState.url
  when (urlProg /= urlState) $ do
    removeDancer theScene dState
    loadModel theScene d.url dState
  pure dState


loadModel :: Three.Scene -> String -> DancerState -> Effect Unit
loadModel theScene url dState = do
  write url dState.url
  let url' = resolveURL url
  _ <- Three.loadGLTF_DRACO "https://dktr0.github.io/LocoMotion/threejs/" url' $ \gltf -> do
    log $ "model " <> url' <> " loaded with " <> show (length gltf.animations) <> " animations"
    Three.printAnything gltf
    Three.addAnything theScene gltf.scene
    mixer <- Three.newAnimationMixer gltf.scene -- make an animation mixer
    clipActions <- traverse (Three.clipAction mixer) gltf.animations -- convert all animations to AnimationActions connected to the animation mixer
    write (Just gltf.scene) dState.theDancer
    write (Just gltf.animations) dState.animations
    write (Just mixer) dState.animationMixer
    write clipActions dState.clipActions
    animIndex <- read dState.prevAnimationIndex
    case animIndex of
      (-9999) -> playAnimation dState 0
      x -> playAnimation dState x
  pure unit


removeDancer :: Three.Scene -> DancerState -> Effect Unit
removeDancer sc d = do
  x <- read d.theDancer
  case x of
    Just y -> Three.removeObject3D sc y
    Nothing -> pure unit


playAnimation :: DancerState -> Int -> Effect Unit
playAnimation dState n = do
  x <- read dState.animationMixer
  case x of
    Just _ -> do
      clipActions <- read dState.clipActions
      let nActions = length clipActions
      prevN <- read dState.prevAnimationIndex
      when ((prevN /= n) && (nActions > 0)) $ do
        let n' = mod n nActions
        case clipActions!!n' of
          Just newAction -> do
            z <- read dState.prevAnimationAction
            case z of
              Just oldAction -> do
                {- Three.setEffectiveWeight newAction 1.0
                Three.setEffectiveTimeScale newAction 1.0
                Three.crossFadeTo oldAction newAction 0.1 true -}
                -- log $ "stopping " <> show prevN <> " and starting " <> show n'
                Three.stop oldAction
                Three.playAnything newAction
              Nothing -> do
                -- log $ "playing newAction " <> show n'
                -- Three.setEffectiveWeight newAction 1.0
                Three.setEffectiveTimeScale newAction 1.0
                Three.playAnything newAction
            write (Just newAction) dState.prevAnimationAction
          Nothing -> log "strange error in LocoMotion - DancerState.purs"
      write n dState.prevAnimationIndex
    Nothing -> pure unit


updateAnimationDuration :: DancerState -> Number -> Effect Unit
updateAnimationDuration dState dur = do
  x <- read dState.prevAnimationAction
  case x of
    Just action -> do
      Three.setDuration action dur
    Nothing -> pure unit
