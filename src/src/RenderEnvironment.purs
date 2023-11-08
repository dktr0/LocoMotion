module RenderEnvironment where

import ThreeJS as Three
import Data.Tempo (Tempo)

type RenderEnvironment = {
  scene :: Three.Scene,
  camera :: Three.PerspectiveCamera,
  fog :: Three.Fog,
  renderer :: Three.Renderer,
  defaultLight :: Three.AmbientLight,
  tempo :: Tempo,
  cycle :: Number, -- duration of cycle in seconds
  time :: Number,
  beat :: Number,
  delta :: Number
  }

