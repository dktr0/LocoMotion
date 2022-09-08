module DancerState (
  DancerState(..),
  MaybeRef(..),
  Model(..),
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

-- like 'when' but specialized for a MaybeRef
whenMaybeRef :: forall a. MaybeRef a -> (a -> Effect Unit) -> Effect Unit
whenMaybeRef mRef f = do
  m <- read mRef
  case m of
    Just a -> f a
    Nothing -> pure unit

type DancerState =
  {
  url :: Ref String,
  model :: MaybeRef Model,
  prevAnimationIndex :: Ref Int,
  prevAnimationAction :: MaybeRef Three.AnimationAction
  }

type Model = {
  scene :: Three.Scene,
  clips :: Array Three.AnimationClip,
  mixer :: Three.AnimationMixer,
  actions :: Array Three.AnimationAction
  }

runDancerWithState :: Three.Scene -> Number -> Number -> Number -> Dancer -> Maybe DancerState -> Effect DancerState
runDancerWithState theScene cycleDur nowCycles delta d maybeDancerState = do
  s <- loadModelIfNecessary theScene d maybeDancerState
  updateTransforms nowCycles d s
  updateAnimation delta cycleDur nowCycles d s
  pure s


loadModelIfNecessary :: Three.Scene -> Dancer -> Maybe DancerState -> Effect DancerState
loadModelIfNecessary theScene d Nothing = do
  url <- new d.url
  model <- new Nothing
  prevAnimationIndex <- new (-9999)
  prevAnimationAction <- new Nothing
  let s = { url, model, prevAnimationIndex, prevAnimationAction }
  loadModel theScene d.url s
  pure s
loadModelIfNecessary theScene d (Just s) = do
  let urlProg = d.url
  urlState <- read s.url
  when (urlProg /= urlState) $ do
    removeDancer theScene s
    loadModel theScene d.url s
  pure s


updateTransforms :: Number -> Dancer -> DancerState -> Effect Unit
updateTransforms nowCycles d s = whenMaybeRef s.model $ \m -> do
  let x'  = sampleVariable nowCycles d.pos.x
  let y'  = sampleVariable nowCycles d.pos.y
  let z'  = sampleVariable nowCycles d.pos.z
  Three.setPositionOfAnything m.scene x' y' z'
  let rx'  = sampleVariable nowCycles d.rot.x
  let ry'  = sampleVariable nowCycles d.rot.y
  let rz'  = sampleVariable nowCycles d.rot.z
  Three.setRotationOfAnything m.scene rx' ry' rz'
  let sx'  = sampleVariable nowCycles d.scale.x
  let sy'  = sampleVariable nowCycles d.scale.y
  let sz'  = sampleVariable nowCycles d.scale.z
  Three.setScaleOfAnything m.scene sx' sy' sz'


updateAnimation :: Number -> Number -> Number -> Dancer -> DancerState -> Effect Unit
updateAnimation delta cycleDur nowCycles d s = whenMaybeRef s.model $ \m -> do
  playAnimation s $ animationExprToIntHack d.animation
  updateAnimationDuration s $ sampleVariable nowCycles d.dur * cycleDur
  Three.updateAnimationMixer m.mixer delta


loadModel :: Three.Scene -> String -> DancerState -> Effect Unit
loadModel theScene url s = do
  write url s.url
  let url' = resolveURL url
  _ <- Three.loadGLTF_DRACO "https://dktr0.github.io/LocoMotion/threejs/" url' $ \gltf -> do
    log $ "model " <> url' <> " loaded with " <> show (length gltf.animations) <> " animations"
    Three.addAnything theScene gltf.scene
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
  pure { scene: gltf.scene, clips: gltf.animations, mixer: mixer, actions: actions }


removeDancer :: Three.Scene -> DancerState -> Effect Unit
removeDancer theScene s = whenMaybeRef s.model $ \m -> Three.removeObject3D theScene m.scene


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
