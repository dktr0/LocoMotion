module RenderEngine
  (
  RenderEngine(..),
  launchRenderEngine,
  setProgram,
  evaluate,
  animate,
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
import ThreeJS as Three
import Web.HTML as HTML
import Web.HTML.Window as HTML
import Web.HTML.HTMLCanvasElement as HTML
import Data.DateTime
import Data.Time.Duration
import Effect.Now (nowDateTime)
import Data.Newtype (unwrap)
import Data.Tempo
import Data.Rational

import AST
import DancerState
import Parser

type RenderEngine =
  {
  launchTime :: DateTime,
  scene :: Three.Scene,
  camera :: Three.PerspectiveCamera,
  renderer :: Three.Renderer,
  programRef :: Ref Program,
  renderState :: Ref RenderState,
  tempo :: Ref Tempo
  }

type RenderState =
  {
  dancers :: Array DancerState
  }

defaultRenderState :: RenderState
defaultRenderState = { dancers:[] }

launchRenderEngine :: HTML.HTMLCanvasElement -> Effect RenderEngine
launchRenderEngine cvs = do
  launchTime <- nowDateTime
  scene <- Three.newScene

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
  camera <- Three.newPerspectiveCamera 45.0 (iWidth/iHeight) 0.1 100.0
  Three.setPositionOfAnything camera 0.0 1.0 5.0

  renderer <- Three.newWebGLRenderer { antialias: true, canvas: cvs }
  Three.setSize renderer iWidth iHeight false

  programRef <- new defaultProgram
  renderState <- new defaultRenderState
  tempo <- newTempo (1 % 2) >>= new
  let re = { launchTime, scene, camera, renderer, programRef, renderState, tempo }
  pure re


setProgram :: RenderEngine -> Program -> Effect Unit
setProgram re p = write p re.programRef


evaluate :: RenderEngine -> String -> Effect (Maybe String)
evaluate re x = do
  case parseProgram x of
    Right p -> do
      setProgram re p
      pure Nothing
    Left err -> pure $ Just err

animate :: RenderEngine -> Effect Unit
animate re = do
  runProgram re
  iWidth <- Three.windowInnerWidth
  iHeight <- Three.windowInnerHeight
  Three.setAspect re.camera (iWidth/iHeight)
  Three.setSize re.renderer iWidth iHeight false
  Three.render re.renderer re.scene re.camera


runProgram :: RenderEngine -> Effect Unit
runProgram re = do
  p <- read re.programRef
  rState <- read re.renderState
  t <- read re.tempo
  now <- nowDateTime
  let nCycles = timeToCount t now
  -- note: it is overkill to regenerate the lists of element states in every frame
  -- later we'll refactor so this only happens in first frame after new program...
  ds <- foldM (runDancers re nCycles rState.dancers) [] p
  -- TODO: need a way of deleting dancers/ethereals when they are removed also...
  write (rState { dancers=ds }) re.renderState
  

runDancers :: RenderEngine -> Rational -> Array DancerState -> Array DancerState -> Statement -> Effect (Array DancerState)
runDancers re nCycles dsPrev dsNew (Element (Dancer d)) = do
  x <- case dsPrev !! length dsNew of
    Just prevDancer -> runDancer nCycles d prevDancer
    Nothing -> addDancer re.scene d
  pure $ snoc dsNew x
runDancers _ _ _ dsNew _ = pure dsNew
