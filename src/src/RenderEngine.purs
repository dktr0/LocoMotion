module RenderEngine
  (
  RenderEngine(..),
  launchRenderEngine,
  setProgram,
  evaluate,
  RenderState(..)
  ) where

import Prelude
import Effect (Effect)
import Effect.Ref (Ref, new, read, write)
import Effect.Console (log)
import Data.Foldable (foldM)
import Data.Array ((!!),length,snoc)
import Data.Maybe
import Data.Either
import Graphics.Three.Scene as Scene
import Graphics.Three.Camera as Camera
import Graphics.Three.Renderer as Renderer
import Graphics.Three.Geometry as Geometry
import Graphics.Three.Material as Material
import Graphics.Three.Object3D as Object3D
import ThreeJS as Three
import Data.Foreign.EasyFFI (unsafeForeignProcedure)

import AST
import DancerState
import Parser

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
  dancers :: Array DancerState
  }

defaultRenderState :: RenderState
defaultRenderState = { dancers:[] }

launchRenderEngine :: Effect RenderEngine
launchRenderEngine = do
  scene <- Scene.create

  hemiLight <- Three.newHemisphereLight 0xffffff 0x444444 2.0
  Three.setPositionOfAnything hemiLight 0.0 20.0 0.0
  Three.addAnythingToScene scene hemiLight

  Three.newAmbientLight 0xffffff 1.0 >>= Three.addAnythingToScene scene

  dirLight <- Three.newDirectionalLight 0x887766 1.0
  Three.setPositionOfAnything dirLight (-1.0) 1.0 1.0
  Three.addAnythingToScene scene dirLight

  pgh <- Three.newPolarGridHelper 10.0 8 8 8
  Three.setPositionOfAnything pgh 0.0 0.0 0.0
  Three.addAnythingToScene scene pgh

  iWidth <- Three.windowInnerWidth
  iHeight <- Three.windowInnerHeight
  camera <- Camera.createPerspective 45.0 (iWidth/iHeight) 0.1 100.0
  Three.setPositionOfAnything camera 0.0 1.0 5.0

  renderer <- Renderer.createWebGL { antialias: true }
  Renderer.setSize renderer iWidth iHeight
  Renderer.appendToDomByID renderer "canvas"

  programRef <- new defaultProgram
  renderState <- new defaultRenderState
  let re = { scene, camera, renderer, programRef, renderState }
  requestAnimationFrame $ animate re
  pure re


setProgram :: RenderEngine -> Program -> Effect Unit
setProgram re p = write p re.programRef


evaluate :: RenderEngine -> String -> Effect String
evaluate re x = do
  case parseProgram x of
    Right p -> do
      setProgram re p
      pure "success!"
    Left err -> pure $ "syntax: " <> err

animate :: RenderEngine -> Effect Unit
animate re = do
  runProgram re
  iWidth <- Three.windowInnerWidth
  iHeight <- Three.windowInnerHeight
  Camera.setAspect re.camera (iWidth/iHeight)
  Renderer.setSize re.renderer iWidth iHeight
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
  -- TODO: need a way of deleting dancers/ethereals when they are removed also...
  write (rState { dancers=ds {- , ethereals=es -} }) re.renderState


runDancers :: RenderEngine -> Array DancerState -> Array DancerState -> Statement -> Effect (Array DancerState)
runDancers re dsPrev dsNew (Element (Dancer d)) = do
  x <- case dsPrev !! length dsNew of
    Just prevDancer -> runDancer d prevDancer
    Nothing -> addDancer re.scene d
  pure $ snoc dsNew x
runDancers _ _ dsNew _ = pure dsNew
