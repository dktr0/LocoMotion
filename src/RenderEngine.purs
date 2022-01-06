module RenderEngine
  (
  RenderEngine(..),
  launchRenderEngine,
  setProgram
  ) where

import Prelude
import Effect (Effect)
import Effect.Ref (Ref, new, read, write)
import Graphics.Three.Scene as Scene
import Graphics.Three.Camera as Camera
import Graphics.Three.Renderer as Renderer
import Graphics.Three.Geometry as Geometry
import Graphics.Three.Material as Material
import Graphics.Three.Object3D as Object3D
import Data.Foreign.EasyFFI (unsafeForeignProcedure)

import Program

type RenderEngine =
  {
  scene :: Scene.Scene,
  camera :: Camera.PerspectiveCamera,
  renderer :: Renderer.Renderer,
  mesh :: Object3D.Mesh,
  programRef :: Ref Program
  }

launchRenderEngine :: Effect RenderEngine
launchRenderEngine = do
  scene <- Scene.create
  camera <- Camera.createPerspective 75.0 (16.0/9.0) 0.1 100.0
  renderer <- Renderer.createWebGL { antialias: true }
  Renderer.setSize renderer 400.0 400.0
  Renderer.appendToDomByID renderer "canvas"
  geometry <- Geometry.createBox 1.0 1.0 1.0
  material <- Material.createMeshBasic { color: "red" }
  mesh <- Object3D.createMesh geometry material
  Scene.addObject scene mesh
  Object3D.setPosition camera 0.0 0.0 5.0
  programRef <- new defaultProgram
  let re = { scene, camera, renderer, mesh, programRef }
  requestAnimationFrame $ animate re
  pure re

setProgram :: RenderEngine -> Program -> Effect Unit
setProgram re p = write p re.programRef

runProgram :: RenderEngine -> Effect Unit
runProgram re = do
  program <- read re.programRef
  Object3D.rotateIncrement re.mesh program program 0.0

animate :: RenderEngine -> Effect Unit
animate re = do
  runProgram re
  Renderer.render re.renderer re.scene re.camera
  requestAnimationFrame $ animate re

requestAnimationFrame :: Effect Unit -> Effect Unit
requestAnimationFrame = unsafeForeignProcedure ["callback", ""] "window.requestAnimationFrame(callback)"
