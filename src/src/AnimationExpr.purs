module AnimationExpr where

import Prelude
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.List
import Data.Tuple
import Parsing.Combinators

import TokenParser

{-

data AnimationExpr =
  AnimationIndexInt Int | -- example: 2
  AnimationIndexString String | -- example: "headroll"
  AnimationMix (List (Tuple AnimationExpr Variable)) -- example: ["headroll" 0.5, "legroll" (osc 0.5 * 0.2)]

derive instance Generic AnimationExpr _

instance Show AnimationExpr where
  show x = genericShow x -- note: point-free not possible because recursive type

defaultAnimationExpr :: AnimationExpr
defaultAnimationExpr = AnimationIndexInt 0

-- this will not be necessary soon
animationExprToIntHack :: AnimationExpr -> Int
animationExprToIntHack (AnimationIndexInt i) = i
animationExprToIntHack _ = 0

animationExprP :: P AnimationExpr
animationExprP = do
  _ <- pure unit
  choice [
    try animationIndexInt,
    try animationIndexString,
    animationMix
    ]

animationIndexInt :: P AnimationExpr
animationIndexInt = AnimationIndexInt <$> integer

animationIndexString :: P AnimationExpr
animationIndexString = AnimationIndexString <$> stringLiteral

animationMix :: P AnimationExpr
animationMix = do
  _ <- pure unit
  AnimationMix <$> (brackets $ commaSep $ animationMixTuple)

animationMixTuple :: P (Tuple AnimationExpr Variable)
animationMixTuple = do
  _ <- pure unit
  Tuple <$> animationExprP <*> variable

-}
