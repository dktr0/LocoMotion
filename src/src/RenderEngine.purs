module RenderEngine
  (
  RenderEngine(..),
  launchRenderEngine,
  setProgram,
  RenderState(..)
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

import AST

type RenderEngine =
  {
  scene :: Scene.Scene,
  camera :: Camera.PerspectiveCamera,
  renderer :: Renderer.Renderer,
  programRef :: Ref Program,
  programState :: Ref RenderState
  }

type RenderState =
  {
  dancerStates :: Array DancerState,
  etherealStates :: Array EtherealState
  }

defaultRenderState :: RenderState
defaultRenderState = { dancers:[], ethereals:[] }

type DancerState =
  {
  mesh :: Object3D.Mesh
  }

type EtherealState =
  {
  mesh :: Object3D.Mesh
  }

launchRenderEngine :: Effect RenderEngine
launchRenderEngine = do
  scene <- Scene.create
  camera <- Camera.createPerspective 75.0 (16.0/9.0) 0.1 100.0
  renderer <- Renderer.createWebGL { antialias: true }
  Renderer.setSize renderer 400.0 400.0
  Renderer.appendToDomByID renderer "canvas"
  Object3D.setPosition camera 0.0 0.0 5.0
  programRef <- new defaultProgram
  programState <- new defaultRenderState
  let re = { scene, camera, renderer, programRef, programState }
  requestAnimationFrame $ animate re
  pure re


setProgram :: RenderEngine -> Program -> Effect Unit
setProgram re p = write p re.programRef


animate :: RenderEngine -> Effect Unit
animate re = do
  runProgram re
  Renderer.render re.renderer re.scene re.camera
  requestAnimationFrame $ animate re

requestAnimationFrame :: Effect Unit -> Effect Unit
requestAnimationFrame = unsafeForeignProcedure ["callback", ""] "window.requestAnimationFrame(callback)"


runProgram :: RenderEngine -> Effect Unit
runProgram re = do
  p <- read re.programRef
  pState <- read re.programState
  -- note: it is overkill to regenerate the lists of element states in every frame
  -- later we'll refactor so this only happens in first frame after new program...
  ds <- foldM (runDancers renderEngine pState.dancerStates) [] p
  -- es <- foldM (runEthereals renderEngine pState.etherealStates) [] p
  -- TODO: need a way of deleting dancers/ethereals when they are removed also...
  write $ pState { dancerStates=dsNew {- , etherealStates=esNew -} }


runDancers :: RenderEngine -> Array DancerState -> Array DancerState -> Statement -> Effect (Array DancerState)
runDancers re dsPrev dsNew (Element (Dancer d)) = do
  -- if this dancer is new (more dancers there than before), add/make a new DancerState
  -- otherwise, just lookup the previously cached state
  -- maybe' :: forall b a. (Unit -> b) -> (a -> b) -> Maybe a -> b
  let prevDState = dsPrev !! length dsNew
  dState' <- maybe' (Unit -> b) pure $ dsPrev !! length dsNew
  dState <- if length dsNew >= length dsPrev then (addDancer re d) else (pure $ )
  -- ...placeholder: later, here, properties of the DancerState can be accessed/changed...
  pure $ snoc dsNew dState
runDancers _ _ dsNew _ = pure dsNew


addDancer :: RenderEngine -> Dancer -> Effect DancerState
addDancer re _ = do
  geometry <- Geometry.createBox 1.0 1.0 1.0
  material <- Material.createMeshBasic { color: "red" }
  mesh <- Object3D.createMesh geometry material
  Scene.addObject re.scene mesh
  pure { mesh }

{-
runEthereals :: RenderEngine -> Array EtherealState -> Array EtherealState-> Statement -> Effect (Array EtherealState)
runEthereals re esPrev esNew (Element (Ethereal d)) = ???
runEthereals _ _ esNew _ = pure esNew
-}

-- runCameraChanges :: RenderEngine -> Statement -> Effect Unit
