module Main where

import Prelude
import Data.Either
import Data.Number
import Data.Maybe
import Effect
import Effect.Class
import Effect.Console (log)
import Web.HTML.HTMLCanvasElement as HTML

import RenderEngine
import AST
import Parser


launch :: HTML.HTMLCanvasElement -> Effect RenderEngine
launch cvs = do
  log "LocoMotion: launch"
  launchRenderEngine cvs


evaluateLocomotion :: RenderEngine -> String -> Effect { success :: Boolean, error :: String }
evaluateLocomotion re x = do
  log "LocoMotion: evaluate"
  y <- evaluate re x
  case y of
    Just error -> pure $ { success: false, error }
    Nothing -> pure $ { success: true, error: "" }


animateLocomotion :: RenderEngine -> Effect Unit
animateLocomotion re = animate re
