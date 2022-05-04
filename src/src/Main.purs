module Main where

import Prelude
import Data.Either
import Data.Number
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


evaluateLocomotion :: RenderEngine -> String -> Effect String
evaluateLocomotion re x = do
  log "LocoMotion: evaluate"
  evaluate re x


animateLocomotion :: RenderEngine -> Effect Unit
animateLocomotion re = animate re


-- we imagine that this PureScript module will also be
-- a module from the standpoint of a JavaScript application as well.
-- main is provided so that spago can bundle an app, and it will
-- run when the resulting app/module is loaded into the DOM - but
-- we make it do nothing (since other definitions are the real
-- entry points into the module). Perhaps it could eventually
-- serve some use in initializing the module though?

main :: Effect Unit
main = pure unit
