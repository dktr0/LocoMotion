module Main where

import Prelude
import Data.Maybe
import Data.Number
import Effect
import Effect.Class
import Effect.Console (log)

import Halogen as H
import Halogen.Aff as H
import Halogen.HTML as H
import Halogen.HTML.Events as H
import Halogen.HTML.Properties as H
import Halogen.VDom.Driver as H
import Web.UIEvent.InputEvent

import RenderEngine
import Program


type EditorState = String -- just an error message for now

initialState :: forall input. input -> EditorState
initialState _ = ""

type EditorAction = String -- represents text to be evaluated/parsed

render :: forall m. EditorState -> H.ComponentHTML EditorAction () m
render st = H.div [ H.class_ $ H.ClassName "editor" ]
  [
    H.div [] [H.text "untitled NFRF project"],
    H.div [] [H.textarea [ H.onValueInput identity ]],
    H.div [] [H.text st]
  ]

handleAction :: forall output m. MonadEffect m => RenderEngine -> EditorAction -> H.HalogenM EditorState EditorAction () output m Unit
handleAction re txt = do
  case parseProgram txt of
    Just p -> do
      H.liftEffect $ setProgram re p
      H.modify_ $ \_ -> "success!"
    Nothing ->
      H.modify_ $ \_ -> "unable to parse"

component :: forall query input output m. MonadEffect m => RenderEngine -> H.Component query input output m
component re =
  H.mkComponent
    {
    initialState,
    render,
    eval: H.mkEval H.defaultEval { handleAction = handleAction re }
    }

main :: Effect Unit
main = do
  log "untitled NFRF project"
  re <- launchRenderEngine
  H.runHalogenAff $ do
    body <- H.awaitBody
    H.runUI (component re) unit body
