module ThreeJS where

-- This module contains extra PureScript bindings for ThreeJS things.

import Prelude
import Effect (Effect)
import Graphics.Three.Scene as Scene
import Graphics.Three.Camera as Camera
import Graphics.Three.Object3D as Object3D

-- import Graphics.Three.Renderer as Renderer
-- import Graphics.Three.Geometry as Geometry
-- import Graphics.Three.Material as Material

-- Loading GLTF resources via GLTFLoader

foreign import data AnimationClip :: Type

type GLTF = {
  animations :: Array AnimationClip, -- in ThreeJS: Array<THREE.AnimationClip>
  scene :: Scene.Scene, -- in ThreeJS: THREE.Group
  scenes :: Array Scene.Scene, -- in ThreeJS: Array<THREE.Group>
  -- cameras :: Array Camera.Camera,
  asset :: Object3D.Mesh -- ? -- in ThreeJS: Object
  }

foreign import loadGLTF :: String -> (GLTF -> Effect Unit) -> Effect Unit

-- hacky, but... for now...
foreign import addAnythingToScene :: forall a. Scene.Scene -> a -> Effect Unit

-- hacky, but... for now...
foreign import setPositionOfAnything :: forall a. a -> Number -> Number -> Number -> Effect Unit

foreign import setRotationOfAnything :: forall a. a -> Number -> Number -> Number -> Effect Unit

foreign import data HemisphereLight :: Type

foreign import newHemisphereLight :: Int -> Int -> Number -> Effect HemisphereLight

foreign import data AmbientLight :: Type

foreign import newAmbientLight :: Int -> Number -> Effect AmbientLight

foreign import data DirectionalLight :: Type

foreign import newDirectionalLight :: Int -> Number -> Effect DirectionalLight

foreign import data PolarGridHelper :: Type

foreign import newPolarGridHelper :: Number -> Int -> Int -> Int -> Effect PolarGridHelper

foreign import windowInnerWidth :: Effect Number

foreign import windowInnerHeight :: Effect Number

foreign import data AnimationMixer :: Type

foreign import newAnimationMixer :: forall o. o -> Effect AnimationMixer

foreign import updateAnimationMixer :: AnimationMixer -> Number -> Effect Unit

foreign import data AnimationAction :: Type

foreign import clipAction :: AnimationMixer -> AnimationClip -> Effect AnimationAction

foreign import setEffectiveTimeScale :: AnimationAction -> Number -> Effect Unit

foreign import play :: forall o. o -> Effect Unit
