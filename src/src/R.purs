module R where

-- R is a monad for describing and composing render-actions
-- which are effects on the 3D/ThreeJS world of LocoMotion
-- statements in the LocoMotion language are translated into such render-actions
-- in other words, a LocoMotion program is a structure of such render-actions
-- they have access to a RenderState (State monad) and RenderEnvironment (Reader monad)

import Control.Monad.State.Trans
import ThreeJS as Three



type RenderEnvironment = {
  scene :: Three.Scene,
  camera :: Three.PerspectiveCamera,
  renderer :: Three.Renderer,
  nCycles :: Number
  }

type RenderState = {
  dancers :: IntMap DancerState,
  floors :: IntMap FloorState
  }

type R a = StateT RenderState (ReaderT RenderEnvironment Effect) a


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
  case Map.lookup k valueMap of
    Just v -> liftIO $ f (valueToNumber v)
    Nothing -> pure unit



-- floor { colour = 0xff00ff }
floor :: Transformer -> R ()


-- lisa = dancer { url="lisa.glb" }
dancer :: Transformer -> R ???


astToProgram :: AST -> R ()
