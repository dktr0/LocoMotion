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
import Data.Tuple

import AST

main :: Effect Unit
main = launchAff_ $ runSpec [consoleReporter] do
  describe "the AST parser" do
    it "parses empty string" $ parseAST "" `shouldEqual` Right emptyAST
    it "parses a semicolon" $ parseAST ";" `shouldEqual` (Right (EmptyStatement ((Position { column: 1, index: 0, line: 1 })) : EmptyStatement ((Position { column: 2, index: 1, line: 1 })) : Nil))
    it "parses just dancer" $ parseAST "dancer" `shouldEqual` (Right (Action (Dancer ((Position { column: 1, index: 0, line: 1 }))) : Nil))
    it "parses just dancer assigned" $ parseAST "x=dancer" `shouldEqual` (Right (Assignment ((Position { column: 1, index: 0, line: 1 })) "x" (Dancer ((Position { column: 3, index: 2, line: 1 }))) : Nil))
    it "parses an int" $ parseAST "3" `shouldEqual` (Right (Action (LiteralInt ((Position { column: 1, index: 0, line: 1 })) 3) : Nil))
    it "parses a number" $ parseAST "3.5" `shouldEqual` (Right (Action (LiteralNumber ((Position { column: 1, index: 0, line: 1 })) 3.5) : Nil))
    it "parses a string literal" $ parseAST "\"literal\"" `shouldEqual` (Right (Action ((LiteralString ((Position { column: 1, index: 0, line: 1 })) "literal")) : Nil))
    it "parses an empty transformer" $ parseAST "{}" `shouldEqual` (Right (Action (Transformer ((Position { column: 1, index: 0, line: 1 })) (Nil)) : Nil))
    it "parses a transformer with a URL" $ parseAST "{ url=\"lisa.glb\"}" `shouldEqual` (Right (Action (Transformer ((Position { column: 1, index: 0, line: 1 })) (((Tuple "url" (LiteralString ((Position { column: 7, index: 6, line: 1 })) "lisa.glb")) : Nil))) : Nil))
    it "parses just osc" $ parseAST "osc" `shouldEqual` (Right (Action (Osc ((Position { column: 1, index: 0, line: 1 }))) : Nil))
    it "parses \"osc 3\"" $ parseAST "osc 3" `shouldEqual` (Right (Action (Application ((Position { column: 1, index: 0, line: 1 })) (Osc ((Position { column: 1, index: 0, line: 1 }))) (LiteralInt ((Position { column: 5, index: 4, line: 1 })) 3)) : Nil))
    it "parses \"range 0 10\"" $ parseAST "range 0 10" `shouldEqual` (Right (Action (Application ((Position { column: 1, index: 0, line: 1 })) (Application ((Position { column: 1, index: 0, line: 1 })) (Range ((Position { column: 1, index: 0, line: 1 }))) (LiteralInt ((Position { column: 7, index: 6, line: 1 })) 0)) (LiteralInt ((Position { column: 9, index: 8, line: 1 })) 10)) : Nil))
    it "parses \"f 1 2 3.5\"" $ parseAST "f 1 2 3.5" `shouldEqual` (Right (Action (Application ((Position { column: 1, index: 0, line: 1 })) (Application ((Position { column: 1, index: 0, line: 1 })) (Application ((Position { column: 1, index: 0, line: 1 })) (SemiGlobal ((Position { column: 1, index: 0, line: 1 })) "f") (LiteralInt ((Position { column: 3, index: 2, line: 1 })) 1)) (LiteralInt ((Position { column: 5, index: 4, line: 1 })) 2)) (LiteralNumber ((Position { column: 7, index: 6, line: 1 })) 3.5)) : Nil))
    it "parses a dancer with a url" $ parseAST "dancer { url=\"lisa.glb\" }" `shouldEqual` (Right (Action (Application ((Position { column: 1, index: 0, line: 1 })) (Dancer ((Position { column: 1, index: 0, line: 1 }))) (Transformer ((Position { column: 8, index: 7, line: 1 })) (((Tuple "url" (LiteralString ((Position { column: 14, index: 13, line: 1 })) "lisa.glb")) : Nil)))) : Nil))
    it "parses a simple clear expression" $ parseAST "clear { colour=0x102030, alpha=0.75 }" `shouldEqual` (Right (Action (Application ((Position { column: 1, index: 0, line: 1 })) (Clear ((Position { column: 1, index: 0, line: 1 }))) (Transformer ((Position { column: 7, index: 6, line: 1 })) (((Tuple "colour" (LiteralInt ((Position { column: 16, index: 15, line: 1 })) 1056816)) : (Tuple "alpha" (LiteralNumber ((Position { column: 32, index: 31, line: 1 })) 0.75)) : Nil)))) : Nil))
