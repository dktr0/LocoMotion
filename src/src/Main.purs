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
import Effect.Ref (write)

import RenderEngine as RE
import AST
import Parser


launch :: HTML.HTMLCanvasElement -> Effect RE.RenderEngine
launch cvs = do
  log "LocoMotion: launch"
  RE.launchRenderEngine cvs


evaluate :: RE.RenderEngine -> String -> Effect { success :: Boolean, error :: String }
evaluate re x = do
  log "LocoMotion: evaluate"
  y <- RE.evaluate re x
  case y of
    Just error -> pure $ { success: false, error }
    Nothing -> pure $ { success: true, error: "" }


animate :: RE.RenderEngine -> Effect Unit
animate re = RE.animate re


setTempo :: RE.RenderEngine -> ForeignTempo -> Effect Unit
setTempo re t = write (fromForeignTempo t) re.tempo
