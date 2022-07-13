module AnimationExpr (
  AnimationExpr(..),
  defaultAnimationExpr,
  animationExprP,
  animationExprToIntHack
  ) where

import Prelude
import Data.List
import Data.Tuple
import Parsing.Combinators

import TokenParser
import Variable

data AnimationExpr =
  AnimationIndexInt Int | -- example: 2
  AnimationIndexString String | -- example: "headroll"
  AnimationMix (List (Tuple AnimationExpr Variable)) -- example: ["headroll" 0.5, "legroll" (osc 0.5 * 0.2)]

instance Show AnimationExpr where
  show (AnimationIndexInt i) = show i
  show (AnimationIndexString s) = show s
  show (AnimationMix l) = show l

defaultAnimationExpr :: AnimationExpr
defaultAnimationExpr = AnimationIndexInt 0

-- this will not be necessary soon
animationExprToIntHack :: AnimationExpr -> Int
animationExprToIntHack (AnimationIndexInt i) = i
animationExprToIntHack _ = 0

animationExprP :: P AnimationExpr
animationExprP = try animationIndexInt

animationIndexInt :: P AnimationExpr
animationIndexInt = AnimationIndexInt <$> integer
