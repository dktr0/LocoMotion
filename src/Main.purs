module Main where

import Prelude
import Data.Maybe
import Effect (Effect)
import Effect.Console (log)
import Data.Foreign.EasyFFI

import Graphics.Three.Scene as Scene
import Graphics.Three.Camera as Camera
import Graphics.Three.Renderer as Renderer
import Graphics.Three.Geometry as Geometry
import Graphics.Three.Material as Material
import Graphics.Three.Object3D as Object3D

import Halogen as H
import Halogen.Aff as H
import Halogen.HTML as H
import Halogen.HTML.Events as H
import Halogen.HTML.Properties as H
import Halogen.VDom.Driver as H
import Web.UIEvent.InputEvent

type State =
  {
  scene :: Scene.Scene,
  camera :: Camera.PerspectiveCamera,
  renderer :: Renderer.Renderer,
  mesh :: Object3D.Mesh
  }

newState :: Effect State
newState = do
  log "newState started..."
  scene <- Scene.create
  camera <- Camera.createPerspective 75.0 (16.0/9.0) 0.1 100.0
  renderer <- Renderer.createWebGL { antialias: true }
  Renderer.setSize renderer 400.0 400.0
  Renderer.appendToDomByID renderer "canvas"
  geometry <- Geometry.createBox 1.0 1.0 1.0
  material <- Material.createMeshBasic { color: "red" }
  mesh <- Object3D.createMesh geometry material
  Scene.addObject scene mesh
  Object3D.setPosition camera 0.0 0.0 5.0
  requestAnimationFrame $ animate renderer scene camera mesh
  log "newState finished."
  pure { scene, camera, renderer, mesh }

main :: Effect Unit
main = do
  log "untitled NFRF project"
  st <- newState
  H.runHalogenAff $ do
    body <- H.awaitBody
    H.runUI (component st) unit body

component :: forall query input output m. State -> H.Component query input output m
component x = H.mkComponent { initialState: initialState x, render, eval: H.mkEval H.defaultEval { handleAction = handleAction } }

initialState :: forall input. State -> input -> State
initialState st _ = st

render :: forall m. State -> H.ComponentHTML Action () m
render st = H.div [ H.class_ $ H.ClassName "editor" ]
  [
    H.text "untitled NFRF project",
    H.textarea []
  ]

type Action = Unit

handleAction :: forall output m. Action -> H.HalogenM State Action () output m Unit
handleAction _ = H.modify_ $ \st -> st


animate :: Renderer.Renderer -> Scene.Scene -> Camera.PerspectiveCamera -> Object3D.Mesh -> Effect Unit
animate renderer scene camera mesh = do
  Object3D.rotateIncrement mesh 0.01 0.01 0.0
  Renderer.render renderer scene camera
  requestAnimationFrame $ animate renderer scene camera mesh
  log "."

requestAnimationFrame :: Effect Unit -> Effect Unit
requestAnimationFrame = unsafeForeignProcedure ["callback", ""] "window.requestAnimationFrame(callback)"
