module RenderEngine
  (
  RenderEngine(..),
  launch,
  evaluate,
  clearZone,
  preAnimate,
  animateZone,
  postAnimate,
  ZoneState(..),
  IntMap(..)
  ) where

import Prelude
import Effect (Effect)
import Effect.Ref (Ref, new, read, write)
import Effect.Console (log)
import Data.Foldable (foldM)
import Data.Map
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
import Data.Tuple
import Data.FunctorWithIndex (mapWithIndex)

import AST
import DancerState
import Parser
import ZoneMap (Zone,ZoneMap)
import ZoneMap as ZoneMap

type RenderEngine =
  {
  launchTime :: DateTime,
  scene :: Three.Scene,
  camera :: Three.PerspectiveCamera,
  renderer :: Three.Renderer,
  programs :: ZoneMap Program,
  zoneStates :: ZoneMap ZoneState,
  tempo :: Ref Tempo
  }

type IntMap a = Map Int a

type ZoneState = {
  dancers :: IntMap DancerState
  }

defaultZoneState :: ZoneState
defaultZoneState = { dancers: empty }

launch :: HTML.HTMLCanvasElement -> Effect RenderEngine
launch cvs = do
  log "LocoMotion: launch"
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

  programs <- ZoneMap.new
  zoneStates <- ZoneMap.new
  tempo <- newTempo (1 % 2) >>= new
  let re = { launchTime, scene, camera, renderer, programs, zoneStates, tempo }
  pure re


evaluate :: RenderEngine -> Int -> String -> Effect (Maybe String)
evaluate re z x = do
  case parseProgram x of
    Right p -> do
      ZoneMap.write z p re.programs
      pure Nothing
    Left err -> pure $ Just err


clearZone :: RenderEngine -> Int -> Effect Unit
clearZone re z = do
  ZoneMap.delete z re.programs
  ZoneMap.delete z re.zoneStates
  log "LocoMotion WARNING: clearZone is not properly implemented yet (needs to delete assets!)"



preAnimate :: RenderEngine -> Effect Unit
preAnimate _ = pure unit


animateZone :: RenderEngine -> Zone -> Effect Unit
animateZone re z = do
  x <- ZoneMap.read z re.programs
  case x of
    Nothing -> do
      log "LocoMotion ERROR: animateZone called for zone with no program"
      pure unit
    Just prog -> do
      y <- ZoneMap.read z re.zoneStates
      let zoneState = case y of
                        Just y' -> y'
                        Nothing -> defaultZoneState
      zoneState' <- runProgram re prog zoneState
      ZoneMap.write z zoneState' re.zoneStates


postAnimate :: RenderEngine -> Effect Unit
postAnimate re = do
  iWidth <- Three.windowInnerWidth
  iHeight <- Three.windowInnerHeight
  Three.setAspect re.camera (iWidth/iHeight)
  Three.setSize re.renderer iWidth iHeight false
  Three.render re.renderer re.scene re.camera


runProgram :: RenderEngine -> Program -> ZoneState -> Effect ZoneState
runProgram re prog zoneState = do
  t <- read re.tempo
  now <- nowDateTime
  let nCycles = timeToCountNumber t now
  let prog' = mapWithIndex Tuple prog
  foldM (runStatement re nCycles) zoneState prog'


runStatement :: RenderEngine -> Number -> ZoneState -> Tuple Int Statement -> Effect ZoneState
runStatement re nCycles zoneState (Tuple stmtIndex (Element e)) = runElement re nCycles stmtIndex e zoneState
runStatement _ _ zoneState _ = pure zoneState


runElement :: RenderEngine -> Number -> Int -> Element -> ZoneState -> Effect ZoneState
runElement re nCycles stmtIndex (Dancer d) zoneState = runDancer re nCycles stmtIndex d zoneState
runElement _ _ _ _ zoneState = pure zoneState


runDancer :: RenderEngine -> Number -> Int -> Dancer -> ZoneState -> Effect ZoneState
runDancer re nCycles stmtIndex d zoneState = do
  let prevDancerState = lookup stmtIndex zoneState.dancers
  ds <- runDancerWithState re.scene nCycles d prevDancerState
  pure $ zoneState { dancers = insert stmtIndex ds zoneState.dancers }
