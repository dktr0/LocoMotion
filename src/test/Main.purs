module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class.Console (log)
import Test.Spec (describe,pending,it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

import Data.List ((:),List(..))
import Data.Either (Either(..))
import Parsing (Position(..))

import AST

main :: Effect Unit
main = launchAff_ $ runSpec [consoleReporter] do
  describe "the AST parser" do
    it "parses empty string" $ parseAST "" `shouldEqual` Right emptyAST
    it "parses a semicolon" $ parseAST ";" `shouldEqual` (Right (EmptyStatement ((Position { column: 1, index: 0, line: 1 })) : EmptyStatement ((Position { column: 2, index: 1, line: 1 })) : Nil))
    it "parses just dancer" $ parseAST "dancer" `shouldEqual` (Right (Action (Dancer ((Position { column: 1, index: 0, line: 1 }))) : Nil))
    it "parses just dancer assigned" $ parseAST "x=dancer" `shouldEqual` (Right (Assignment ((Position { column: 1, index: 0, line: 1 })) "x" (Dancer ((Position { column: 3, index: 2, line: 1 }))) : Nil))
    it "parses an int" $ parseAST "3" `shouldEqual` (Right (Action (LiteralNumber ((Position { column: 1, index: 0, line: 1 })) 3.0) : Nil))
