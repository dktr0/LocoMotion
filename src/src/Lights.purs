module Lights
  (
  newAmbient,updateAmbient,removeAmbient
  )
  where

import Prelude
import ThreeJS as Three
import Control.Monad.Reader.Trans (ask)
import Effect.Class (liftEffect)
import Effect.Console (log)

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
  let colour = lookupInt 0xffffff "colour" vm
  intensity <- realizeNumber "intensity" 1.0 vm
  liftEffect $ Three.setColorInt x.ambientLight colour
  liftEffect $ Three.setLightIntensity x.ambientLight intensity
  pure x

removeAmbient :: Ambient -> R Unit
removeAmbient x = liftEffect $ Three.removeFromParent x.ambientLight
