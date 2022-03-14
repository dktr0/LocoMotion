module ThreeJS where

-- This module contains extra PureScript bindings for ThreeJS things.

import Prelude
import Effect (Effect)
import Graphics.Three.Scene as Scene
-- import Graphics.Three.Camera as Camera
-- import Graphics.Three.Renderer as Renderer
-- import Graphics.Three.Geometry as Geometry
-- import Graphics.Three.Material as Material
-- import Graphics.Three.Object3D as Object3D

-- Loading GLTF resources via GLTFLoader

type GLTF = {
  -- animations :: ?, -- in ThreeJS: Array<THREE.AnimationClip>
  scene :: Scene.Scene -- in ThreeJS: THREE.Group
  -- scenes :: ?, -- in ThreeJS: Array<THREE.Group>
  -- cameras :: ?, -- in ThreeJS: Array<THREE.Camera>
  -- asset :: ? -- in ThreeJS: Object
  }

foreign import loadGLTF :: String -> (GLTF -> Effect Unit) -> Effect Unit

-- hacky, but... for now...
foreign import addAnythingToScene :: forall a. Scene.Scene -> a -> Effect Unit

-- hacky, but... for now...
foreign import setPositionOfAnything :: forall a. a -> Number -> Number -> Number -> Effect Unit

foreign import data HemisphereLight :: Type

foreign import newHemisphereLight :: Int -> Int -> Number -> Effect HemisphereLight
