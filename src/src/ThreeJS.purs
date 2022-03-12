module ThreeJS where

import Prelude
import Effect (Effect)
import Graphics.Three.Scene as Scene
import Graphics.Three.Camera as Camera
import Graphics.Three.Renderer as Renderer
import Graphics.Three.Geometry as Geometry
import Graphics.Three.Material as Material
import Graphics.Three.Object3D as Object3D
import Data.Foreign.EasyFFI (unsafeForeignFunction,unsafeForeignProcedure)

foreign import data GLTFLoader :: Type

newGLTFLoader :: Effect GLTFLoader
newGLTFLoader = unsafeForeignFunction [""] "new THREE.GLTFLoader"
