module Lights
  (
  newAmbient,updateAmbient,removeAmbient,
  newDirectional,updateDirectional,removeDirectional,
  newHemisphere,updateHemisphere,removeHemisphere,
  newPoint,updatePoint,removePoint,
  newRectArea,updateRectArea,removeRectArea,
  newSpot,updateSpot,removeSpot
  )
  where

import Prelude
import ThreeJS as Three
import Control.Monad.Reader.Trans (ask)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Data.Number (pi)

import R
import Value


newAmbient :: R Ambient
newAmbient = do
  env <- ask
  liftEffect $ do
    ambientLight <- Three.newAmbientLight 0x000000 0.0
    Three.addAnything env.scene ambientLight
    pure { ambientLight }

updateAmbient :: ValueMap -> Ambient -> R Ambient
updateAmbient vm x = do
  updateColourAndIntensity vm x.ambientLight
  pure x

removeAmbient :: Ambient -> R Unit
removeAmbient x = do
  rEnv <- ask
  liftEffect $ Three.removeObject3D rEnv.scene x.ambientLight


updateColourAndIntensity :: forall a. Three.Light a => ValueMap -> a -> R Unit
updateColourAndIntensity vm a = do
  let colour = lookupInt 0xffffff "colour" vm
  intensity <- realizeNumber "intensity" 1.0 vm
  liftEffect $ Three.setColorInt a colour
  liftEffect $ Three.setLightIntensity a intensity


newDirectional :: R Directional
newDirectional = do
  env <- ask
  liftEffect $ do
    directionalLight <- Three.newDirectionalLight 0x000000 0.0
    Three.addAnything env.scene directionalLight
    virtualTarget <- Three.newObject3D
    Three.addAnything env.scene virtualTarget
    Three.setTarget directionalLight virtualTarget
    pure { directionalLight, virtualTarget }

updateDirectional :: ValueMap -> Directional -> R Directional
updateDirectional vm x = do
  updateColourAndIntensity vm x.directionalLight
  updatePosition vm x.directionalLight
  updateVirtualTarget vm x.virtualTarget
  pure x

removeDirectional :: Directional -> R Unit
removeDirectional x = do
  rEnv <- ask
  liftEffect $ do
    Three.removeObject3D rEnv.scene x.directionalLight
    Three.removeObject3D x.directionalLight x.virtualTarget


updateVirtualTarget :: forall t. Three.Object3D' t => ValueMap -> t -> R Unit
updateVirtualTarget vm t = do
  lx <- realizeNumber "lx" 0.0 vm
  ly <- realizeNumber "ly" 0.0 vm
  lz <- realizeNumber "lz" 0.0 vm
  liftEffect $ Three.setPosition t lx ly lz


newHemisphere :: R Hemisphere
newHemisphere = do
  env <- ask
  liftEffect $ do
    hemisphereLight <- Three.newHemisphereLight 0x000000 0xffffff 0.0
    Three.addAnything env.scene hemisphereLight
    pure { hemisphereLight }

updateHemisphere :: ValueMap -> Hemisphere -> R Hemisphere
updateHemisphere vm x = do
  let groundColour = lookupInt 0xffffff "ground" vm
  liftEffect $ Three.setGroundColor x.hemisphereLight groundColour
  updateColourAndIntensity vm x.hemisphereLight
  updatePosition vm x.hemisphereLight
  pure x

removeHemisphere :: Hemisphere -> R Unit
removeHemisphere x = do
  rEnv <- ask
  liftEffect $ Three.removeObject3D rEnv.scene x.hemisphereLight


newPoint :: R Point
newPoint = do
  env <- ask
  liftEffect $ do
    pointLight <- Three.newPointLight 0x000000 0.0 0.0 2.0
    Three.addAnything env.scene pointLight
    pure { pointLight }

updatePoint :: ValueMap -> Point -> R Point
updatePoint vm x = do
  distance <- realizeNumber "distance" 0.0 vm
  decay <- realizeNumber "decay" 2.0 vm
  liftEffect $ do
    Three.setDistance x.pointLight distance
    Three.setDecay x.pointLight decay
  updateColourAndIntensity vm x.pointLight
  updatePosition vm x.pointLight
  pure x

removePoint :: Point -> R Unit
removePoint x = do
  rEnv <- ask
  liftEffect $ Three.removeObject3D rEnv.scene x.pointLight


newRectArea :: R RectArea
newRectArea = do
  env <- ask
  liftEffect $ do
    rectAreaLight <- Three.newRectAreaLight 0x000000 0.0 10.0 10.0
    Three.addAnything env.scene rectAreaLight
    pure { rectAreaLight }

updateRectArea :: ValueMap -> RectArea -> R RectArea
updateRectArea vm x = do
  width <- realizeNumber "width" 10.0 vm
  height <- realizeNumber "height" 10.0 vm
  liftEffect $ do
    Three.setWidth x.rectAreaLight width
    Three.setHeight x.rectAreaLight height
  updateColourAndIntensity vm x.rectAreaLight
  updatePosition vm x.rectAreaLight
  updateScale vm x.rectAreaLight
  updateRotation vm x.rectAreaLight
  pure x

removeRectArea :: RectArea -> R Unit
removeRectArea x = do
  rEnv <- ask
  liftEffect $ Three.removeObject3D rEnv.scene x.rectAreaLight


newSpot :: R Spot
newSpot = do
  env <- ask
  liftEffect $ do
    spotLight <- Three.newSpotLight 0xffffff 0.0 0.0 (pi/2.0) 0.0 0.0
    Three.addAnything env.scene spotLight
    virtualTarget <- Three.newObject3D
    Three.addAnything env.scene virtualTarget
    Three.setTarget spotLight virtualTarget
    pure { spotLight, virtualTarget }

updateSpot :: ValueMap -> Spot -> R Spot
updateSpot vm x = do
  distance <- realizeNumber "distance" 0.0 vm
  angle <- realizeNumber "angle" 90.0 vm
  penumbra <- realizeNumber "penumbra" 0.0 vm
  decay <- realizeNumber "decay" 0.0 vm
  liftEffect $ do
    Three.setDistance x.spotLight distance
    Three.setAngle x.spotLight $ angle * pi / 180.0
    Three.setPenumbra x.spotLight penumbra
    Three.setDecay x.spotLight decay
  updateColourAndIntensity vm x.spotLight
  updatePosition vm x.spotLight
  updateVirtualTarget vm x.virtualTarget
  pure x

removeSpot :: Spot -> R Unit
removeSpot x = do
  rEnv <- ask
  liftEffect $ do
    Three.removeObject3D rEnv.scene x.spotLight
    Three.removeObject3D rEnv.scene x.virtualTarget
