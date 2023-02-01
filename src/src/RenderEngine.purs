module RenderEngine
  (
  RenderEngine(..),
  launch,
  evaluate,
  clearZone,
  preAnimate,
  animateZone,
  postAnimate
  ) where

import Prelude
import Effect (Effect)
import Effect.Ref (Ref, new, read, write)
import Effect.Console (log)
import Data.Array (length,drop,take,index,insertAt)
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
import Data.TraversableWithIndex (traverseWithIndex)
import Data.Traversable (traverse_)
import Control.Monad.State (get,modify_)
import Control.Monad.Reader (ask)
import Effect.Class (liftEffect)

import Value
import Parser
import DancerState
import FloorState
import ZoneMap (Zone,ZoneMap)
import ZoneMap as ZoneMap
import R
import Program

type RenderEngine =
  {
  renderEnvironment :: Ref RenderEnvironment,
  programs :: ZoneMap Program,
  zoneStates :: ZoneMap ZoneState,
  prevTNow :: Ref DateTime
  }


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

  tempo <- newTempo (1 % 2)
  let nCycles = 0.0
  let cycleDur = 2.0
  let delta = 0.0
  renderEnvironment <- new { scene, camera, renderer, tempo, nCycles, cycleDur, delta }
  programs <- ZoneMap.new
  zoneStates <- ZoneMap.new
  prevTNow <- nowDateTime >>= new
  pure { renderEnvironment, programs, zoneStates, prevTNow }


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
  envPrev <- read re.renderEnvironment
  let envNew = envPrev {
    delta = unwrap (diff tNow tPrev :: Seconds),
    nCycles = timeToCountNumber envPrev.tempo tNow,
    cycleDur = 1.0 / toNumber envPrev.tempo.freq
    }
  write envNew re.renderEnvironment


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
      rEnv <- read re.renderEnvironment
      zoneState' <- runProgram rEnv prog zoneState
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
    rEnv <- read re.renderEnvironment
    Three.setAspect rEnv.camera (iWidth/iHeight)
    Three.setSize rEnv.renderer iWidth iHeight false
    Three.render rEnv.renderer rEnv.scene rEnv.camera
  -- t1 <- nowDateTime
  -- let tDiff = unwrap (diff t1 t0 :: Milliseconds)
  -- log $ "postAnimate " <> show tDiff


runProgram :: RenderEnvironment -> Program -> ZoneState -> Effect ZoneState
runProgram re prog zoneState = execR re zoneState $ do
  runDancers prog.dancers
  runFloors prog.floors
  runCamera prog.cameraMap


runDancers :: Array ValueMap -> R Unit
runDancers xs = do
  -- run/update active dancers
  _ <- traverseWithIndex runDancer xs
  let nDancers = length xs
  -- remove any deleted dancers
  s <- get
  traverse_ removeDancer $ drop nDancers s.dancers
  modify_ $ \x -> x { dancers = take nDancers x.dancers }


runDancer :: Int -> ValueMap -> R Unit
runDancer i vm = do
  s <- get
  updatedDancerState <- runDancerWithState vm (index s.dancers i)
  modify_ $ \x -> x { dancers = insertAt' i updatedDancerState x.dancers }


insertAt' :: forall a. Int -> a -> Array a -> Array a
insertAt' i v a = case insertAt i v a of
  Just a' -> a'
  Nothing -> a

runFloors :: Array ValueMap -> R Unit
runFloors xs = do
  -- run/update active dancers
  _ <- traverseWithIndex runFloor xs
  let nFloors = length xs
  -- remove any deleted floors
  s <- get
  traverse_ removeFloor $ drop nFloors s.floors
  modify_ $ \x -> x { floors = take nFloors x.floors }


runFloor :: Int -> ValueMap -> R Unit
runFloor i vm = do
  s <- get
  updatedFloorState <- runFloorWithState vm (index s.floors i)
  modify_ $ \x -> x { floors = insertAt' i updatedFloorState x.floors }


runCamera :: ValueMap -> R Unit
runCamera vm = do
  re <- ask
  setCameraProperty "x" 0.0 vm (Three.setPositionX re.camera)
  setCameraProperty "y" 1.0 vm (Three.setPositionY re.camera)
  setCameraProperty "z" 10.0 vm (Three.setPositionZ re.camera)
  setCameraProperty "rx" 0.0 vm (Three.setRotationX re.camera)
  setCameraProperty "ry" 0.0 vm (Three.setRotationY re.camera)
  setCameraProperty "rz" 0.0 vm (Three.setRotationZ re.camera)

setCameraProperty :: String -> Number -> ValueMap -> (Number -> Effect Unit) -> R Unit
setCameraProperty k d vm f = do
  n <- realizeNumber k d vm
  liftEffect $ f n
