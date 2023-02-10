module R where

-- R is a monad for describing and composing render-actions
-- which are effects on the 3D/ThreeJS world of LocoMotion
-- statements in the LocoMotion language are translated into such render-actions
-- in other words, a LocoMotion program is a structure of such render-actions
-- they have access to a RenderState (State monad) and RenderEnvironment (Reader monad)

import Prelude (bind,($),pure,mod)
import Effect (Effect)
import Control.Monad.State.Trans
import Control.Monad.Reader.Trans
import ThreeJS as Three
import Data.Tempo (Tempo)
import Effect.Ref (Ref)
import Data.Array (replicate,updateAt)
import Data.Maybe (fromMaybe)

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
  mixer :: Three.AnimationMixer,
  actions :: Array Three.AnimationAction,
  mixerState :: Ref MixerState
  }

type MixerState = Array Number

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
