module RenderEngine
  (
  RenderEngine(..),
  launchRenderEngine,
  setProgram,
  RenderState(..),
  DancerState,
  EtherealState
  ) where

import Prelude
import Effect (Effect)
import Effect.Ref (Ref, new, read, write)
import Data.Foldable (foldM)
import Data.Array ((!!),length,snoc)
import Data.Maybe
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
  renderState :: Ref RenderState
  }

type RenderState =
  {
  dancers :: Array DancerState,
  ethereals :: Array EtherealState
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
  renderState <- new defaultRenderState
  let re = { scene, camera, renderer, programRef, renderState }
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
  rState <- read re.renderState
  -- note: it is overkill to regenerate the lists of element states in every frame
  -- later we'll refactor so this only happens in first frame after new program...
  ds <- foldM (runDancers re rState.dancers) [] p
  -- es <- foldM (runEthereals re rState.ethereals) [] p
  -- TODO: need a way of deleting dancers/ethereals when they are removed also...
  write (rState { dancers=ds {- , ethereals=es -} }) re.renderState


runDancers :: RenderEngine -> Array DancerState -> Array DancerState -> Statement -> Effect (Array DancerState)
runDancers re dsPrev dsNew (Element (Dancer d)) = do
  x <- case dsPrev !! length dsNew of
    Just prevDancer -> runDancer re d prevDancer
    Nothing -> addDancer re d
  pure $ snoc dsNew x
runDancers _ _ dsNew _ = pure dsNew

addDancer :: RenderEngine -> Dancer -> Effect DancerState
addDancer re _ = do
  geometry <- Geometry.createBox 1.0 1.0 1.0
  material <- Material.createMeshBasic { color: "red" }
  mesh <- Object3D.createMesh geometry material
  Scene.addObject re.scene mesh
  pure { mesh }

runDancer :: RenderEngine -> Dancer -> DancerState -> Effect DancerState
runDancer _ _ dState = pure dState -- placeholder

{-
runEthereals :: RenderEngine -> Array EtherealState -> Array EtherealState-> Statement -> Effect (Array EtherealState)
runEthereals re esPrev esNew (Element (Ethereal d)) = ???
runEthereals _ _ esNew _ = pure esNew
-}

-- runCameraChanges :: RenderEngine -> Statement -> Effect Unit
