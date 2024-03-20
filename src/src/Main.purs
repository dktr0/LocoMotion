module Main where

import Prelude
import Data.Either
import Data.Number
import Data.Maybe
import Effect
import Effect.Class
import Effect.Console (log)
import Web.HTML.HTMLCanvasElement as HTML
import Data.Tempo
import Effect.Ref (write,read)

import RenderEngine as RE
import AST
import Parser


launch :: HTML.HTMLCanvasElement -> Effect RE.RenderEngine
launch = RE.launch


define :: RE.RenderEngine -> Int -> Number -> String -> Effect { success :: Boolean, error :: String }
define re zone eTime x = do
  y <- RE.define re zone eTime x
  case y of
    Just error -> pure $ { success: false, error }
    Nothing -> pure $ { success: true, error: "" }


clear :: RE.RenderEngine -> Int -> Effect Unit
clear = RE.clear


setTempo :: RE.RenderEngine -> ForeignTempo -> Effect Unit
setTempo re t = do
  rEnv <- read re.renderEnvironment
  -- log $ "foreignTempo: " <> show t <> ", tempo: " <> show (fromForeignTempo t)
  write (rEnv { tempo = fromForeignTempo t } ) re.renderEnvironment


preRender :: RE.RenderEngine -> Number -> Effect Unit
preRender = RE.preRender


render :: RE.RenderEngine -> Number -> Int -> Effect Unit
render = RE.render


postRender :: RE.RenderEngine -> Number -> Effect Unit
postRender = RE.postRender
