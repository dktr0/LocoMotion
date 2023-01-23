module R where

-- R is a monad for describing and composing render-actions
-- which are effects on the 3D/ThreeJS world of LocoMotion
-- statements in the LocoMotion language are translated into such render-actions
-- in other words, a LocoMotion program is a structure of such render-actions
-- they have access to a RenderState (State monad) and RenderEnvironment (Reader monad)

import Prelude (bind,($),pure)
import Effect (Effect)
import Data.Map
import Control.Monad.State.Trans
import Control.Monad.Reader.Trans
import ThreeJS as Three
import Data.Tempo (Tempo)

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

type RenderState = {
  -- dancers :: Map Int DancerState,
  -- floors :: Map Int FloorState
  }

type R a = StateT RenderState (ReaderT RenderEnvironment Effect) a


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

{-
-- camera { x = 12, z = 10 }
camera :: ValueMap -> R ()
camera valueMap = do
  c <- asks camera
  maybeSetCameraProperty "x" valueMap (Three.setPositionX c)
  maybeSetCameraProperty "y" valueMap (Three.setPositionY c)
  maybeSetCameraProperty "z" valueMap (Three.setPositionZ c)
  maybeSetCameraProperty "rx" valueMap (Three.setRotationX c)
  maybeSetCameraProperty "ry" valueMap (Three.setRotationY c)
  maybeSetCameraProperty "rz" valueMap (Three.setRotationZ c)

maybeSetCameraProperty :: String -> ValueMap -> (Number -> Effect Unit) -> R Unit
maybeSetCameraProperty k valueMap f = do
  case lookup k valueMap of
    Just v -> liftIO $ f (valueToNumber v)
    Nothing -> pure unit
-}
