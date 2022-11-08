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
import Data.Foldable (foldM,foldl)
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

import AST
import Transformer
import Value
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
  floors :: IntMap FloorState,
  semiGlobalMap :: Map.Map String ValueExpr
  }

defaultZoneState :: ZoneState
defaultZoneState = { dancers: Map.empty, floors: Map.empty, semiGlobalMap: Map.empty }

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
  Three.setPositionOfAnything camera 0.0 1.0 10.0

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
      collectSemiGlobals re z p -- *** TODO: evaluation should not succeed when there are infinite reference chains in semiGlobals
      log $ show p
      pure Nothing
    Left err -> pure $ Just err

collectSemiGlobals :: RenderEngine -> Int -> Program -> Effect Unit
collectSemiGlobals re z xs = do
  let xs' = foldl collectSemiGlobal Map.empty xs -- :: Map.Map String ValueExpr
  y <- ZoneMap.read z re.zoneStates
  let zoneState = (maybe defaultZoneState identity y) { semiGlobalMap = xs' }
  ZoneMap.write z zoneState re.zoneStates

collectSemiGlobal :: Map.Map String ValueExpr -> Statement -> Map.Map String ValueExpr
collectSemiGlobal m (SemiGlobal k v) = Map.insert k v m
collectSemiGlobal m _ = m

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

runStatement re cycleDur nCycles delta stmtIndex zoneState (Dancer t) = do
  let prevDancerState = Map.lookup stmtIndex zoneState.dancers
  ds <- runDancerWithState re.scene cycleDur nCycles zoneState.semiGlobalMap delta t prevDancerState
  pure $ zoneState { dancers = Map.insert stmtIndex ds zoneState.dancers }

runStatement re cycleDur nCycles delta stmtIndex zoneState (Floor t) = do
  let prevFloorState = Map.lookup stmtIndex zoneState.floors
  fState <- case prevFloorState of
    Just fState -> do
      runFloorState nCycles zoneState.semiGlobalMap t fState
      pure fState
    Nothing -> do
      fState <- newFloorState re.scene nCycles zoneState.semiGlobalMap t
      pure fState
  pure $ zoneState { floors = Map.insert stmtIndex fState zoneState.floors }

runStatement re cycleDur nCycles delta _ zoneState (Camera t) = do
  let valueMap = realizeTransformer nCycles zoneState.semiGlobalMap t
  maybeSetCameraProperty "x" valueMap (Three.setPositionX re.camera)
  maybeSetCameraProperty "y" valueMap (Three.setPositionY re.camera)
  maybeSetCameraProperty "z" valueMap (Three.setPositionZ re.camera)
  maybeSetCameraProperty "rx" valueMap (Three.setRotationX re.camera)
  maybeSetCameraProperty "ry" valueMap (Three.setRotationY re.camera)
  maybeSetCameraProperty "rz" valueMap (Three.setRotationZ re.camera)
  pure zoneState

runStatement _ _ _ _ _ zoneState _ = pure zoneState


maybeSetCameraProperty :: String -> ValueMap -> (Number -> Effect Unit) -> Effect Unit
maybeSetCameraProperty k valueMap f = do
  case Map.lookup k valueMap of
    Just v -> f (valueToNumber v)
    Nothing -> pure unit
