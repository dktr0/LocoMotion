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
import Data.Array (length,drop,take,index,updateAt,snoc)
import Data.Foldable (foldM,foldl,elem)
import Data.Number (pi)
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
import ZoneMap (Zone,ZoneMap)
import ZoneMap as ZoneMap
import R
import Program
import ElementType
import Dancer
import Plane
import Lights

type RenderEngine =
  {
  renderEnvironment :: Ref RenderEnvironment,
  programs :: ZoneMap Program,
  zoneStates :: ZoneMap ZoneState,
  prevTNow :: Ref DateTime
  }


launch :: HTML.HTMLCanvasElement -> Effect RenderEngine
launch cvs = do
  log "LocoMotion: launch..."
  scene <- Three.newScene

  defaultLight <- Three.newAmbientLight 0xffffff 0.0
  Three.addAnything scene defaultLight

  iWidth <- Three.windowInnerWidth
  iHeight <- Three.windowInnerHeight
  camera <- Three.newPerspectiveCamera 45.0 (iWidth/iHeight) 0.1 100.0
  Three.setPosition camera 0.0 1.0 10.0

  renderer <- Three.newWebGLRenderer { antialias: true, canvas: cvs, alpha: true }
  Three.setSize renderer iWidth iHeight false
  setPixelRatioToDevicePixelRatio renderer
  Three.setClearColor renderer 0x000000 1.0

  tempo <- newTempo (1 % 2)
  let nCycles = 0.0
  let cycleDur = 2.0
  let delta = 0.0
  renderEnvironment <- new { scene, camera, renderer, defaultLight, tempo, nCycles, cycleDur, delta }
  programs <- ZoneMap.new
  zoneStates <- ZoneMap.new
  prevTNow <- nowDateTime >>= new
  log "LocoMotion: launch completed"
  pure { renderEnvironment, programs, zoneStates, prevTNow }


evaluate :: RenderEngine -> Int -> String -> Effect (Maybe String)
evaluate re z x = do
  -- x' <- parseProgramDebug x
  let x' = parseProgram x
  case x' of
    Right p -> do
      ZoneMap.write z p re.programs
      -- log "evaluate completed with no error"
      pure Nothing
    Left err -> do
      -- log "evaluate completed with error"
      pure $ Just err


clearZone :: RenderEngine -> Int -> Effect Unit
clearZone re z = do
  mzs <- ZoneMap.read z re.zoneStates
  case mzs of 
    Just zs -> deleteZoneState zs
    Nothing -> pure unit
  ZoneMap.delete z re.programs
  ZoneMap.delete z re.zoneStates
  log "LocoMotion clearZone"


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
      zoneState' <- runProgram z rEnv prog zoneState
      ZoneMap.write z zoneState' re.zoneStates
  -- t1 <- nowDateTime
  -- let tDiff = unwrap (diff t1 t0 :: Milliseconds)
  -- log $ "animateZone " <> show tDiff


postAnimate :: RenderEngine -> Effect Unit
postAnimate re = do
  -- t0 <- nowDateTime
  n <- ZoneMap.count re.zoneStates
  when (n > 0) $ do
    handleDefaultLighting re
    iWidth <- Three.windowInnerWidth
    iHeight <- Three.windowInnerHeight
    rEnv <- read re.renderEnvironment
    Three.setAspect rEnv.camera (iWidth/iHeight)
    Three.setSize rEnv.renderer iWidth iHeight false
    setClearColor re
    Three.render rEnv.renderer rEnv.scene rEnv.camera
  -- t1 <- nowDateTime
  -- let tDiff = unwrap (diff t1 t0 :: Milliseconds)
  -- log $ "postAnimate " <> show tDiff

handleDefaultLighting :: RenderEngine -> Effect Unit
handleDefaultLighting re = do
  env <- read re.renderEnvironment
  allPrograms <- read re.programs
  let anyCustomLights = elem true $ map _.hasCustomLights allPrograms
  let intensity = if anyCustomLights then 0.0 else 1.0
  Three.setLightIntensity env.defaultLight intensity


setClearColor :: RenderEngine -> Effect Unit
setClearColor re = do
  zs <- read re.programs -- :: Map Int Program
  let clearMaps = Map.catMaybes $ map (_.clearMap) zs -- Map Int ValueMap
  let cm = Map.unions clearMaps
  let c = case Map.lookup "colour" cm of
            Just x -> valueToInt x
            Nothing -> 0x000000
  let a = case Map.lookup "alpha" cm of
            Just x -> valueToNumber x
            Nothing -> 1.0
  rEnv <- read re.renderEnvironment
  Three.setClearColor rEnv.renderer c a

runProgram :: Int -> RenderEnvironment -> Program -> ZoneState -> Effect ZoneState
runProgram zone re prog zoneState = execR re zoneState $ do
  runElements zone prog.elements
  runCamera prog.cameraMap


runElements :: Int -> Array (Tuple ElementType ValueMap) -> R Unit
runElements zone xs = do
  _ <- traverseWithIndex (runElement zone) xs
  let nElements = length xs
  -- remove any deleted elements
  s <- get
  traverse_ removeElement $ drop nElements s.elements
  modify_ $ \x -> x { elements = take nElements x.elements }


-- bizarre that something like this function doesn't seem to exist in the PureScript library
replaceAt :: forall a. Int -> a -> Array a -> Array a
replaceAt i v a
  | i >= length a = snoc a v
  | otherwise = fromMaybe a $ updateAt i v a


runCamera :: ValueMap -> R Unit
runCamera vm = do
  re <- ask
  updatePosition vm re.camera
  updateScale vm re.camera
  updateRotation vm re.camera


runElement :: Int -> Int -> Tuple ElementType ValueMap -> R Unit
runElement zone i (Tuple t vm) = do
  s <- get
  newE <- case index s.elements i of
    Nothing -> do
      e <- createElement t
      updateElement zone vm e
    Just e -> do
      case t == elementType e of
        true -> updateElement zone vm e
        false -> do
          removeElement e
          e' <- createElement t
          updateElement zone vm e'
  modify_ $ \x -> x { elements = replaceAt i newE x.elements }

createElement :: ElementType -> R Element
createElement Dancer = ElementDancer <$> newDancer
createElement Plane = ElementPlane <$> newPlane
createElement Ambient = ElementAmbient <$> newAmbient
createElement Directional = ElementDirectional <$> newDirectional
createElement Hemisphere = ElementHemisphere <$> newHemisphere
createElement Point = ElementPoint <$> newPoint
createElement RectArea = ElementRectArea <$> newRectArea
createElement Spot = ElementSpot <$> newSpot

updateElement :: Int -> ValueMap -> Element -> R Element
updateElement zone vm (ElementDancer x) = updateDancer zone vm x >>= (pure <<< ElementDancer)
updateElement _ vm (ElementPlane x) = updatePlane vm x >>= (pure <<< ElementPlane)
updateElement _ vm (ElementAmbient x) = updateAmbient vm x >>= (pure <<< ElementAmbient)
updateElement _ vm (ElementDirectional x) = updateDirectional vm x >>= (pure <<< ElementDirectional)
updateElement _ vm (ElementHemisphere x) = updateHemisphere vm x >>= (pure <<< ElementHemisphere)
updateElement _ vm (ElementPoint x) = updatePoint vm x >>= (pure <<< ElementPoint)
updateElement _ vm (ElementRectArea x) = updateRectArea vm x >>= (pure <<< ElementRectArea)
updateElement _ vm (ElementSpot x) = updateSpot vm x >>= (pure <<< ElementSpot)

removeElement :: Element -> R Unit
removeElement (ElementDancer x) = removeDancer x
removeElement (ElementPlane x) = removePlane x
removeElement (ElementAmbient x) = removeAmbient x
removeElement (ElementDirectional x) = removeDirectional x
removeElement (ElementHemisphere x) = removeHemisphere x
removeElement (ElementPoint x) = removePoint x
removeElement (ElementRectArea x) = removeRectArea x
removeElement (ElementSpot x) = removeSpot x
