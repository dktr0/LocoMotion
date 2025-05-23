module RenderEngine where

import Prelude (Unit, bind, bottom, discard, map, otherwise, pure, unit, when, ($), (*), (/), (<$>), (<<<), (==), (>), (>=), (>>=), (-))
import Effect (Effect)
import Effect.Ref (Ref, new, read, write)
import Effect.Console (log)
import Data.Array (length,drop,take,index,updateAt,snoc)
import Data.Foldable (elem, foldM)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Either (Either(..))
import ThreeJS as Three
import Web.HTML.HTMLCanvasElement as HTML
import Data.DateTime (DateTime, diff)
import Data.Time.Duration (Milliseconds(..), Seconds)
import Data.DateTime.Instant (instant, toDateTime)
import Effect.Now (nowDateTime)
import Data.Newtype (unwrap)
import Data.Tempo (newTempo, origin, timeToCountNumber)
import Data.Rational (toNumber, (%))
import Data.Tuple (Tuple(..))
import Data.TraversableWithIndex (traverseWithIndex)
import Data.Traversable (traverse_)
import Control.Monad.State (get,modify_)

import Value (ValueMap, valueToInt, valueToNumber)
import Parser (parseProgram)
import ZoneMap (Zone,ZoneMap)
import ZoneMap as ZoneMap
import R (Element(..), R, ZoneState, defaultZoneState, elementType, execR, setPixelRatioToDevicePixelRatio, updateCameraProperties, updatePosition, updateRotation, updateScale)
import Program (Program, defaultCamera, defaultClear)
import ElementType (ElementType(..))
import Dancer (newDancer, removeDancer, updateDancer)
import Plane (newPlane, removePlane, updatePlane)
import Shapes (newBox, newSphere, removeBox, removeSphere, updateBox, updateSphere)
import Lights (newAmbient, newDirectional, newHemisphere, newPoint, newRectArea, newSpot, removeAmbient, removeDirectional, removeHemisphere, removePoint, removeRectArea, removeSpot, updateAmbient, updateDirectional, updateHemisphere, updatePoint, updateRectArea, updateSpot)
import RenderEnvironment (RenderEnvironment)

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

  fog <- Three.newFog 0xffffff 1000.0 1000.0
  Three.setFog scene fog

  renderer <- Three.newWebGLRenderer { antialias: true, canvas: cvs, alpha: true }
  Three.setSize renderer iWidth iHeight false
  setPixelRatioToDevicePixelRatio renderer
  Three.setClearColor renderer 0x000000 1.0

  tempo <- newTempo (1 % 2)
  let cycle = 2.0
  let time = 0.0
  let beat = 0.0
  let eTime = 0.0
  let eBeat = 0.0
  let delta = 0.0
  renderEnvironment <- new { scene, camera, fog, renderer, defaultLight, tempo, time, beat, eTime, eBeat, cycle, delta }
  programs <- ZoneMap.new
  zoneStates <- ZoneMap.new
  prevTNow <- nowDateTime >>= new
  log "LocoMotion: launch completed"
  pure { renderEnvironment, programs, zoneStates, prevTNow }


define :: RenderEngine -> Int -> Number -> String -> Effect (Maybe String)
define re z eTime x = do
  -- x' <- parseProgramDebug x
  let x' = parseProgram eTime x
  case x' of
    Right p -> do
      ZoneMap.write z p re.programs
      pure Nothing
    Left err -> pure $ Just err


clear :: RenderEngine -> Int -> Effect Unit
clear re z = do
  mzs <- ZoneMap.read z re.zoneStates
  case mzs of
    Just zs -> do
      rEnv <- read re.renderEnvironment
      deleteZoneState rEnv zs
    Nothing -> pure unit
  ZoneMap.delete z re.programs
  ZoneMap.delete z re.zoneStates
  log "LocoMotion clear"
  -- TODO: if there are no active zones left, the canvas should be cleared to transparent somehow


preRender :: RenderEngine -> Number -> Effect Unit
preRender re tNowNumber = do
  let tNow = numberToDateTime tNowNumber
  tPrev <- read re.prevTNow
  write tNow re.prevTNow
  envPrev <- read re.renderEnvironment
  let beatNow = timeToCountNumber envPrev.tempo tNow
  let envNew = envPrev {
    time = unwrap (diff tNow (origin envPrev.tempo) :: Seconds),
    beat = beatNow,
    cycle = 1.0 / toNumber envPrev.tempo.freq,
    delta = unwrap (diff tNow tPrev :: Seconds)
    }
  write envNew re.renderEnvironment


render :: RenderEngine -> Number -> Zone -> Effect Unit
render re tNowNumber z = do
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
      let eTime = tNowNumber - prog.eTime
      let rEnv' = rEnv {
        eTime = eTime,
        eBeat = eTime / toNumber rEnv.tempo.freq
        }
      zoneState' <- runProgram z rEnv' prog zoneState
      ZoneMap.write z zoneState' re.zoneStates
  -- t1 <- nowDateTime
  -- let tDiff = unwrap (diff t1 t0 :: Milliseconds)
  -- log $ "animateZone " <> show tDiff


postRender :: RenderEngine -> Effect Unit
postRender re = do
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
    runCamera re
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
  let clears = map (_.clear) zs
  let clear' = foldM (\vm f -> f vm) defaultClear clears -- :: Either ParseError ValueMap
  let cm = case clear' of
             Right x -> x
             Left _ -> defaultClear -- note: silent failure when there is a ParseError in combining clears
  let c = case Map.lookup "colour" cm of
            Just x -> valueToInt x
            Nothing -> 0x000000
  let a = case Map.lookup "alpha" cm of
            Just x -> valueToNumber x
            Nothing -> 1.0
  rEnv <- read re.renderEnvironment
  Three.setClearColor rEnv.renderer c a


runCamera :: RenderEngine -> Effect Unit
runCamera re = do
  zs <- read re.programs
  rEnv <- read re.renderEnvironment
  let cameras = map (_.camera) zs
  let camera' = foldM (\vm f -> f vm) defaultCamera cameras -- :: Either ParseError ValueMap
  let vm = case camera' of
             Right x -> x
             Left _ -> defaultCamera -- note: silent failure when there is a ParseError in combining cameras
  _ <- execR rEnv defaultZoneState $ do -- a bit hacky but should work since zoneState not relevant to computations
    updatePosition vm rEnv.camera
    updateScale vm rEnv.camera
    updateRotation vm rEnv.camera
    updateCameraProperties vm
  pure unit


runProgram :: Int -> RenderEnvironment -> Program -> ZoneState -> Effect ZoneState
runProgram zone re prog zoneState = execR re zoneState $ runElements zone prog.elements


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
createElement Box = ElementBox <$> newBox
createElement Sphere = ElementSphere <$> newSphere
createElement Ambient = ElementAmbient <$> newAmbient
createElement Directional = ElementDirectional <$> newDirectional
createElement Hemisphere = ElementHemisphere <$> newHemisphere
createElement Point = ElementPoint <$> newPoint
createElement RectArea = ElementRectArea <$> newRectArea
createElement Spot = ElementSpot <$> newSpot

updateElement :: Int -> ValueMap -> Element -> R Element
updateElement zone vm (ElementDancer x) = updateDancer zone vm x >>= (pure <<< ElementDancer)
updateElement _ vm (ElementPlane x) = updatePlane vm x >>= (pure <<< ElementPlane)
updateElement _ vm (ElementBox x) = updateBox vm x >>= (pure <<< ElementBox)
updateElement _ vm (ElementSphere x) = updateSphere vm x >>= (pure <<< ElementSphere)
updateElement _ vm (ElementAmbient x) = updateAmbient vm x >>= (pure <<< ElementAmbient)
updateElement _ vm (ElementDirectional x) = updateDirectional vm x >>= (pure <<< ElementDirectional)
updateElement _ vm (ElementHemisphere x) = updateHemisphere vm x >>= (pure <<< ElementHemisphere)
updateElement _ vm (ElementPoint x) = updatePoint vm x >>= (pure <<< ElementPoint)
updateElement _ vm (ElementRectArea x) = updateRectArea vm x >>= (pure <<< ElementRectArea)
updateElement _ vm (ElementSpot x) = updateSpot vm x >>= (pure <<< ElementSpot)

removeElement :: Element -> R Unit
removeElement (ElementDancer x) = removeDancer x
removeElement (ElementPlane x) = removePlane x
removeElement (ElementBox x) = removeBox x
removeElement (ElementSphere x) = removeSphere x
removeElement (ElementAmbient x) = removeAmbient x
removeElement (ElementDirectional x) = removeDirectional x
removeElement (ElementHemisphere x) = removeHemisphere x
removeElement (ElementPoint x) = removePoint x
removeElement (ElementRectArea x) = removeRectArea x
removeElement (ElementSpot x) = removeSpot x

deleteZoneState :: RenderEnvironment -> ZoneState -> Effect Unit
deleteZoneState re x = do
  _ <- execR re x $ traverse_ removeElement x.elements
  pure unit
  
  
numberToDateTime :: Number -> DateTime
numberToDateTime x = case instant (Milliseconds (x*1000.0)) of
  Just i -> toDateTime i
  Nothing -> toDateTime bottom
