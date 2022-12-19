module R where

-- R is a monad for describing and composing render-actions
-- which are effects on the 3D/ThreeJS world of LocoMotion
-- statements in the LocoMotion language are translated into such render-actions
-- in other words, a LocoMotion program is a structure of such render-actions
-- they have access to a RenderState (State monad) and RenderEnvironment (Reader monad)

import ThreeJS as Three


type RenderEnvironment = {
  _scene :: Three.Scene,
  _camera :: Three.PerspectiveCamera,
  _renderer :: Three.Renderer
  }

type RenderState = {
  dancers :: IntMap DancerState,
  floors :: IntMap FloorState
  }


scene :: R Three.Scene
scene = asks _scene

camera :: R Three.PerspectiveCamera
camera = asks _camera

renderer :: R Three.Renderer
renderer = asks _renderer


-- camera { x = 12, z = 10 }
camera :: Transformer -> R ()


-- floor { colour = 0xff00ff }
floor :: Transformer -> R ()


-- lisa = dancer { url="lisa.glb" }
dancer :: Transformer -> R ???
