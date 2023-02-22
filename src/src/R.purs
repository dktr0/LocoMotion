module R where

-- R is a monad for describing and composing render-actions
-- which are effects on the 3D/ThreeJS world of LocoMotion
-- statements in the LocoMotion language are translated into such render-actions
-- in other words, a LocoMotion program is a structure of such render-actions
-- they have access to a RenderState (State monad) and RenderEnvironment (Reader monad)

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Effect.Class (liftEffect)
import Control.Monad.State.Trans
import Control.Monad.Reader.Trans
import ThreeJS as Three
import Data.Tempo (Tempo)
import Effect.Ref (Ref)
import Data.Array (replicate,updateAt,length,elemIndex)
import Data.Maybe (fromMaybe,isJust)
import Data.Int (floor)
import Data.Map as Map
import Data.Number (pi)

import MaybeRef
import Variable
import Value


type RenderEnvironment = {
  scene :: Three.Scene,
  camera :: Three.PerspectiveCamera,
  renderer :: Three.Renderer,
  tempo :: Tempo,
  nCycles :: Number,
  cycleDur :: Number,
  delta :: Number
  }


type ZoneState = {
  dancers :: Array DancerState,
  floors :: Array FloorState
  }

defaultZoneState :: ZoneState
defaultZoneState = { dancers: [], floors: [] }


type DancerState =
  {
  url :: Ref String,
  model :: MaybeRef Model
  }

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

type FloorState = {
  mesh :: Three.Mesh,
  material :: Three.MeshPhongMaterial
  }

type R a = StateT ZoneState (ReaderT RenderEnvironment Effect) a

execR :: forall a. RenderEnvironment -> ZoneState -> R a -> Effect ZoneState
execR rEnv zState r = runReaderT (execStateT r zState) rEnv

-- looks up the specified entry in the value map
-- if not found, then the provided default is returned
-- if found, and it is a Variable, the variable is realized with respect to the environment
-- if found, and it is some other type, valueToNumber is used to cast it appropriately
realizeNumber :: String -> Number -> ValueMap -> R Number
realizeNumber k def valueMap = do
  let v = lookupValue (ValueNumber def) k valueMap
  case v of
    ValueVariable x -> do
      env <- ask
      pure $ realizeVariable env.nCycles x
    _ -> pure $ valueToNumber v


updateTransforms :: forall a. Three.Object3D a => ValueMap -> a -> R Unit
updateTransforms vm a = do
  -- position
  x <- realizeNumber "x" 0.0 vm
  y <- realizeNumber "y" 0.0 vm
  z <- realizeNumber "z" 0.0 vm
  liftEffect $ Three.setPosition a x y z
  -- scale
  sx <- realizeNumber "sx" 1.0 vm
  sy <- realizeNumber "sy" 1.0 vm
  sz <- realizeNumber "sz" 1.0 vm
  size <- realizeNumber "size" 1.0 vm
  liftEffect $ Three.setScaleOfAnything a (sx*size) (sy*size) (sz*size)
  -- rotation (and lookAt behaviour)
  let mlx = Map.lookup "lx" vm
  let mly = Map.lookup "ly" vm
  let mlz = Map.lookup "lz" vm
  case isJust mlx || isJust mly || isJust mlz of
    true -> do
      lx <- realizeNumber "lx" 0.0 vm
      ly <- realizeNumber "ly" 0.0 vm
      lz <- realizeNumber "lz" 0.0 vm
      -- liftEffect $ log $ "lx ly lz " <> show lx <> " " <> show ly <> " " <> show lz
      rxDelta <- realizeNumber "rx" 0.0 vm
      ryDelta <- realizeNumber "ry" 0.0 vm
      rzDelta <- realizeNumber "rz" 0.0 vm
      liftEffect $ Three.lookAt a lx ly lz
      rx0 <- liftEffect $ Three.getRotationX a
      ry0 <- liftEffect $ Three.getRotationY a
      rz0 <- liftEffect $ Three.getRotationZ a
      let rx = rx0+(rxDelta*pi/180.0)
      let ry = ry0+(ryDelta*pi/180.0)
      let rz = rz0+(rzDelta*pi/180.0)
      liftEffect $ Three.setRotation a rx ry rz
    false -> do
      rx <- realizeNumber "rx" 0.0 vm
      ry <- realizeNumber "ry" 0.0 vm
      rz <- realizeNumber "rz" 0.0 vm
      let rx' = rx*pi/180.0
      let ry' = ry*pi/180.0
      let rz' = rz*pi/180.0
      liftEffect $ Three.setRotation a rx' ry' rz'
