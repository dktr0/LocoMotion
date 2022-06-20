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
import Data.Map as Map
import Data.Maybe
import Data.Either
import Data.List as List
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
import Data.FoldableWithIndex (foldWithIndexM)
import Data.Traversable (traverse_)

import Variable
import AST
import Parser
import DancerState
import FloorState
import ZoneMap (Zone,ZoneMap)
import ZoneMap as ZoneMap

type RenderEngine =
  {
  scene :: Three.Scene,
  camera :: Three.PerspectiveCamera,
  renderer :: Three.Renderer,
  programs :: ZoneMap Program,
  zoneStates :: ZoneMap ZoneState,
  tempo :: Ref Tempo,
  prevTNow :: Ref DateTime,
  delta :: Ref Number
  }

type IntMap a = Map.Map Int a

type ZoneState = {
  dancers :: IntMap DancerState,
  floors :: IntMap FloorState
  }

defaultZoneState :: ZoneState
defaultZoneState = { dancers: Map.empty, floors: Map.empty }

launch :: HTML.HTMLCanvasElement -> Effect RenderEngine
launch cvs = do
  log "LocoMotion: launch"
  scene <- Three.newScene

  hemiLight <- Three.newHemisphereLight 0xffffff 0x444444 0.8
  Three.setPositionOfAnything hemiLight 0.0 20.0 0.0
  Three.addAnythingToScene scene hemiLight
  {- Three.newAmbientLight 0xffffff 0.1 >>= Three.addAnythingToScene scene -}
  dirLight <- Three.newDirectionalLight 0xffffff 0.8
  Three.setPositionOfAnything dirLight (-1.0) 1.0 10.0
  Three.addAnythingToScene scene dirLight

  {- pgh <- Three.newPolarGridHelper 10.0 8 8 8
  Three.setPositionOfAnything pgh 0.0 0.0 0.0
  Three.addAnythingToScene scene pgh -}

  iWidth <- Three.windowInnerWidth
  iHeight <- Three.windowInnerHeight
  camera <- Three.newPerspectiveCamera 45.0 (iWidth/iHeight) 0.1 100.0
  Three.setPositionOfAnything camera 0.0 1.0 5.0

  renderer <- Three.newWebGLRenderer { antialias: true, canvas: cvs }
  Three.setSize renderer iWidth iHeight false

  programs <- ZoneMap.new
  zoneStates <- ZoneMap.new
  tempo <- newTempo (1 % 2) >>= new
  tNow <- nowDateTime
  prevTNow <- new tNow
  delta <- new 0.0
  let re = { scene, camera, renderer, programs, zoneStates, tempo, prevTNow, delta }
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
preAnimate re = do
  tNow <- nowDateTime
  tPrev <- read re.prevTNow
  write tNow re.prevTNow
  write (unwrap (diff tNow tPrev :: Seconds)) re.delta


animateZone :: RenderEngine -> Zone -> Effect Unit
animateZone re z = do
  -- t0 <- nowDateTime
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
  -- t1 <- nowDateTime
  -- let tDiff = unwrap (diff t1 t0 :: Milliseconds)
  -- log $ "animateZone " <> show tDiff


postAnimate :: RenderEngine -> Effect Unit
postAnimate re = do
  -- t0 <- nowDateTime
  n <- ZoneMap.count re.zoneStates
  when (n > 0) $ do
    iWidth <- Three.windowInnerWidth
    iHeight <- Three.windowInnerHeight
    Three.setAspect re.camera (iWidth/iHeight)
    Three.setSize re.renderer iWidth iHeight false
    Three.render re.renderer re.scene re.camera
  -- t1 <- nowDateTime
  -- let tDiff = unwrap (diff t1 t0 :: Milliseconds)
  -- log $ "postAnimate " <> show tDiff


runProgram :: RenderEngine -> Program -> ZoneState -> Effect ZoneState
runProgram re prog zoneState = do
  tNow <- read re.prevTNow
  tempo <- read re.tempo
  let cycleDur = 1.0 / toNumber tempo.freq
  let nCycles = timeToCountNumber tempo tNow
  delta <- read re.delta
  zoneState' <- foldWithIndexM (runStatement re cycleDur nCycles delta) zoneState prog
  removeDeletedElements re prog zoneState'


removeDeletedElements :: RenderEngine -> Program -> ZoneState -> Effect ZoneState
removeDeletedElements re prog zoneState = do
  a <- removeDeletedDancers re prog zoneState
  removeDeletedFloors re prog a

removeDeletedDancers :: RenderEngine -> Program -> ZoneState -> Effect ZoneState
removeDeletedDancers re prog zoneState = do
  let progDancers = Map.filter isDancer prog
  traverse_ (removeDancer re.scene) $ Map.difference zoneState.dancers progDancers -- remove dancers that are in zoneState but not program
  pure $ zoneState { dancers = Map.intersection zoneState.dancers progDancers } -- leave dancers that in both zoneState AND program

removeDeletedFloors :: RenderEngine -> Program -> ZoneState -> Effect ZoneState
removeDeletedFloors re prog zoneState = do
  let progFloors = Map.filter isFloor prog
  traverse_ removeFloorState $ Map.difference zoneState.floors progFloors -- remove dancers that are in zoneState but not program
  pure $ zoneState { floors = Map.intersection zoneState.floors progFloors } -- leave floors that in both zoneState AND program


runStatement :: RenderEngine -> Number -> Number -> Number -> Int -> ZoneState -> Statement -> Effect ZoneState
runStatement re cycleDur nCycles delta stmtIndex zoneState (Dancer d) = runDancer re cycleDur nCycles delta stmtIndex d zoneState
runStatement re cycleDur nCycles delta stmtIndex zoneState (Floor f) = runFloor re cycleDur nCycles delta stmtIndex f zoneState
runStatement re cycleDur nCycles delta _ zoneState (Camera cs) = runCameras re cycleDur nCycles delta cs *> pure zoneState
runStatement _ _ _ _ _ zoneState _ = pure zoneState


runDancer :: RenderEngine -> Number -> Number -> Number -> Int -> Dancer -> ZoneState -> Effect ZoneState
runDancer re cycleDur nCycles delta stmtIndex d zoneState = do
  let prevDancerState = Map.lookup stmtIndex zoneState.dancers
  ds <- runDancerWithState re.scene cycleDur nCycles delta d prevDancerState
  pure $ zoneState { dancers = Map.insert stmtIndex ds zoneState.dancers }


runFloor :: RenderEngine -> Number -> Number -> Number -> Int -> Floor -> ZoneState -> Effect ZoneState
runFloor re cycleDur nCycles delta stmtIndex f zoneState = do
  let prevFloorState = Map.lookup stmtIndex zoneState.floors
  fState <- case prevFloorState of
    Just fState -> do
      runFloorState f fState
      pure fState
    Nothing -> do
      fState <- newFloorState re.scene f
      pure fState
  pure $ zoneState { floors = Map.insert stmtIndex fState zoneState.floors }


runCameras :: RenderEngine -> Number -> Number -> Number -> List.List Camera -> Effect Unit
runCameras re cycleDur nCycles delta cs = traverse_ (runCamera re cycleDur nCycles delta) cs

runCamera :: RenderEngine -> Number -> Number -> Number -> Camera  -> Effect Unit
runCamera re cycleDur nCycles delta (CameraX v) = Three.setPositionX re.camera $ sampleVariable nCycles v
runCamera re cycleDur nCycles delta (CameraY v) = Three.setPositionY re.camera $ sampleVariable nCycles v
runCamera re cycleDur nCycles delta (CameraZ v) = Three.setPositionZ re.camera $ sampleVariable nCycles v
runCamera re cycleDur nCycles delta (CameraRotX v) = Three.setRotationX re.camera $ sampleVariable nCycles v
runCamera re cycleDur nCycles delta (CameraRotY v) = Three.setRotationY re.camera $ sampleVariable nCycles v
runCamera re cycleDur nCycles delta (CameraRotZ v) = Three.setRotationZ re.camera $ sampleVariable nCycles v
