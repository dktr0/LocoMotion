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


newDancerState :: String -> Effect DancerState
newDancerState x = do
  url <- new x
  theDancer <- new Nothing
  animations <- new Nothing
  animationMixer <- new Nothing
  clipActions <- new []
  prevAnimationIndex <- new (-9999)
  prevAnimationAction <- new Nothing
  pure { url, theDancer, animations, animationMixer, clipActions, prevAnimationIndex, prevAnimationAction }


runDancerWithState :: Three.Scene -> Number -> Number -> Dancer -> Maybe DancerState -> Effect DancerState
runDancerWithState theScene nCycles delta d maybeDancerState = do
  dState <- case maybeDancerState of
    Nothing -> addDancer theScene d
    Just x -> do
      updateModelIfNecessary theScene d x
      pure x
  playAnimation dState d.animation
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
    log $ "model " <> url' <> " loaded with " <> show (length gltf.animations) <> " animations"
    Three.addAnything theScene gltf.scene
    mixer <- Three.newAnimationMixer gltf.scene -- make an animation mixer
    clipActions <- traverse (Three.clipAction mixer) gltf.animations -- convert all animations to AnimationActions connected to the animation mixer
    -- traverse_ (flip Three.setEffectiveWeight $ 0.0) clipActions -- set weight of all actions to 0 initially (not sure if this is right....?)
    -- traverse_ Three.playAnything clipActions -- play/activate all of the animation actions
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
                {- log $ "crossfading from " <> show prevN <> " to " <> show n'
                Three.setEffectiveWeight newAction 1.0
                Three.setEffectiveTimeScale newAction 1.0
                Three.crossFadeTo oldAction newAction 0.1 true -}
                log $ "stopping " <> show prevN <> " and starting " <> show n'
                Three.stop oldAction
                Three.playAnything newAction
              Nothing -> do
                log $ "playing newAction " <> show n'
                -- Three.setEffectiveWeight newAction 1.0
                Three.setEffectiveTimeScale newAction 1.0
                Three.playAnything newAction
            write (Just newAction) dState.prevAnimationAction
          Nothing -> log "strange error in LocoMotion - DancerState.purs"
      write n dState.prevAnimationIndex
    Nothing -> pure unit
