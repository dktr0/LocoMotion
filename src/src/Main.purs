module Main where

import Prelude
import Data.Either
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
import AST
import Parser

exampleProgram :: String
exampleProgram = """-- Welcome to LocoMotion
-- A live coding language for dance, choreography, motion, etc
-- Type programs here and click the "Eval" button to make them go
-- This is a rapidly emerging work in progress - check back often for updates
-- Or get in touch on the Estuary discord server!
-- The line below is a valid LocoMotion program (just click Eval!)
dancer { ry=3.14 }; dancer { x = -2 }; dancer { x = 2, ry=1.57 }"""


type EditorState = { text :: String, status :: String }

initialState :: forall input. input -> EditorState
initialState _ = { text: exampleProgram , status: "" }

data EditorAction = TextChanged String | Evaluate

render :: forall m. EditorState -> H.ComponentHTML EditorAction () m
render st = H.div [ H.class_ $ H.ClassName "editorAndStatus" ]
  [
    H.div [ H.class_ $ H.ClassName "editor"]
      [
      H.textarea [
        H.class_ $ H.ClassName "editorArea",
        H.onValueInput TextChanged,
        H.value st.text]
      ],
    H.div [ H.class_ $ H.ClassName "status" ]
      [
      H.button [ H.onClick $ \_ -> Evaluate] [ H.text "eval"],
      H.span [ H.class_ $ H.ClassName "errors" ] [H.text st.status]
      ]
  ]

handleAction :: forall output m. MonadEffect m => RenderEngine -> EditorAction -> H.HalogenM EditorState EditorAction () output m Unit
handleAction re ea = case ea of
  TextChanged t -> H.modify_ $ \st -> st { text = t }
  Evaluate -> do
    st <- H.get
    case parseProgram st.text of
      Right p -> do
        H.liftEffect $ setProgram re p
        H.modify_ _ { status = "success!" }
      Left err -> H.modify_ _ { status = "syntax: " <> err }

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
