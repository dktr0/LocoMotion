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


evaluate :: RE.RenderEngine -> Int -> String -> Effect { success :: Boolean, error :: String }
evaluate re zone x = do
  y <- RE.evaluate re zone x
  case y of
    Just error -> pure $ { success: false, error }
    Nothing -> pure $ { success: true, error: "" }


clearZone :: RE.RenderEngine -> Int -> Effect Unit
clearZone = RE.clearZone


setTempo :: RE.RenderEngine -> ForeignTempo -> Effect Unit
setTempo re t = do
  rEnv <- read re.renderEnvironment
  write (rEnv { tempo = fromForeignTempo t } ) re.renderEnvironment


preAnimate :: RE.RenderEngine -> Effect Unit
preAnimate = RE.preAnimate


animateZone :: RE.RenderEngine -> Int -> Effect Unit
animateZone = RE.animateZone


postAnimate :: RE.RenderEngine -> Effect Unit
postAnimate = RE.postAnimate
