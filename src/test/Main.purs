module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class.Console (log)
import Test.Spec (describe,pending,it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

import Data.Maybe (Maybe(..))
import Data.List ((:),List(..))
import Data.Either (Either(..))
import Parsing (Position(..))
import Data.Tuple
import Data.Map (fromFoldable)

import AST
import Parser
import Program
import Value
import Variable as Variable

main :: Effect Unit
main = pure unit

{- launchAff_ $ runSpec [consoleReporter] do
  describe "the AST parser" do
    it "parses empty string" $ parseAST "" `shouldEqual` Right emptyAST
    it "parses a semicolon" $ parseAST ";" `shouldEqual` (Right (EmptyStatement ((Position { column: 1, index: 0, line: 1 })) : EmptyStatement ((Position { column: 2, index: 1, line: 1 })) : Nil))
    it "parses just dancer" $ parseAST "dancer" `shouldEqual` (Right (Action (Dancer ((Position { column: 1, index: 0, line: 1 }))) : Nil))
    it "parses just dancer assigned" $ parseAST "x=dancer" `shouldEqual` (Right (Assignment ((Position { column: 1, index: 0, line: 1 })) "x" (Dancer ((Position { column: 3, index: 2, line: 1 }))) : Nil))
    it "parses an int" $ parseAST "3" `shouldEqual` (Right (Action (LiteralInt ((Position { column: 1, index: 0, line: 1 })) 3) : Nil))
    it "parses a negative int" $ parseAST "-3" `shouldEqual` (Right (Action (LiteralInt ((Position { column: 1, index: 0, line: 1 })) (-3)) : Nil))
    it "parses a number" $ parseAST "3.5" `shouldEqual` (Right (Action (LiteralNumber ((Position { column: 1, index: 0, line: 1 })) 3.5) : Nil))
    it "parses a negative number" $ parseAST "-3.5" `shouldEqual` (Right (Action (LiteralNumber ((Position { column: 1, index: 0, line: 1 })) (-3.5)) : Nil))
    it "parses a string literal" $ parseAST "\"literal\"" `shouldEqual` (Right (Action ((LiteralString ((Position { column: 1, index: 0, line: 1 })) "literal")) : Nil))
    it "parses an empty transformer" $ parseAST "{}" `shouldEqual` (Right (Action (Transformer ((Position { column: 1, index: 0, line: 1 })) (Nil)) : Nil))
    it "parses a transformer with a URL" $ parseAST "{ url=\"lisa.glb\"}" `shouldEqual` (Right (Action (Transformer ((Position { column: 1, index: 0, line: 1 })) (((Tuple "url" (LiteralString ((Position { column: 7, index: 6, line: 1 })) "lisa.glb")) : Nil))) : Nil))
    it "parses just osc" $ parseAST "osc" `shouldEqual` (Right (Action (Osc ((Position { column: 1, index: 0, line: 1 }))) : Nil))
    it "parses \"osc 3\"" $ parseAST "osc 3" `shouldEqual` (Right (Action (Application ((Position { column: 1, index: 0, line: 1 })) (Osc ((Position { column: 1, index: 0, line: 1 }))) (LiteralInt ((Position { column: 5, index: 4, line: 1 })) 3)) : Nil))
    it "parses \"range 0 10\"" $ parseAST "range 0 10" `shouldEqual` (Right (Action (Application ((Position { column: 1, index: 0, line: 1 })) (Application ((Position { column: 1, index: 0, line: 1 })) (Range ((Position { column: 1, index: 0, line: 1 }))) (LiteralInt ((Position { column: 7, index: 6, line: 1 })) 0)) (LiteralInt ((Position { column: 9, index: 8, line: 1 })) 10)) : Nil))
    it "parses \"f 1 2 3.5\"" $ parseAST "f 1 2 3.5" `shouldEqual` (Right (Action (Application ((Position { column: 1, index: 0, line: 1 })) (Application ((Position { column: 1, index: 0, line: 1 })) (Application ((Position { column: 1, index: 0, line: 1 })) (SemiGlobal ((Position { column: 1, index: 0, line: 1 })) "f") (LiteralInt ((Position { column: 3, index: 2, line: 1 })) 1)) (LiteralInt ((Position { column: 5, index: 4, line: 1 })) 2)) (LiteralNumber ((Position { column: 7, index: 6, line: 1 })) 3.5)) : Nil))
    it "parses a sum of functions" $ parseAST "osc 1 + osc 2" `shouldEqual`  (Right (Action (Sum ((Position { column: 7, index: 6, line: 1 })) (Application ((Position { column: 1, index: 0, line: 1 })) (Osc ((Position { column: 1, index: 0, line: 1 }))) (LiteralInt ((Position { column: 5, index: 4, line: 1 })) 1)) (Application ((Position { column: 9, index: 8, line: 1 })) (Osc ((Position { column: 9, index: 8, line: 1 }))) (LiteralInt ((Position { column: 13, index: 12, line: 1 })) 2))) : Nil))
    it "parses a product of functions" $ parseAST "osc 1 * osc 2" `shouldEqual` (Right (Action (Product ((Position { column: 7, index: 6, line: 1 })) (Application ((Position { column: 1, index: 0, line: 1 })) (Osc ((Position { column: 1, index: 0, line: 1 }))) (LiteralInt ((Position { column: 5, index: 4, line: 1 })) 1)) (Application ((Position { column: 9, index: 8, line: 1 })) (Osc ((Position { column: 9, index: 8, line: 1 }))) (LiteralInt ((Position { column: 13, index: 12, line: 1 })) 2))) : Nil))
    it "parses a dancer with a url" $ parseAST "dancer { url=\"lisa.glb\" }" `shouldEqual` (Right (Action (Application ((Position { column: 1, index: 0, line: 1 })) (Dancer ((Position { column: 1, index: 0, line: 1 }))) (Transformer ((Position { column: 8, index: 7, line: 1 })) (((Tuple "url" (LiteralString ((Position { column: 14, index: 13, line: 1 })) "lisa.glb")) : Nil)))) : Nil))
    it "parses a simple clear expression" $ parseAST "clear { colour=0x102030, alpha=0.75 }" `shouldEqual` (Right (Action (Application ((Position { column: 1, index: 0, line: 1 })) (Clear ((Position { column: 1, index: 0, line: 1 }))) (Transformer ((Position { column: 7, index: 6, line: 1 })) (((Tuple "colour" (LiteralInt ((Position { column: 16, index: 15, line: 1 })) 1056816)) : (Tuple "alpha" (LiteralNumber ((Position { column: 32, index: 31, line: 1 })) 0.75)) : Nil)))) : Nil))
  describe "the AST-to-program parser" do
    it "parses the empty string" $ parseProgram "" `shouldEqual` Right defaultProgram
    it "parses a semicolon" $ parseProgram ";" `shouldEqual` Right defaultProgram
    it "parses just dancer" $ parseProgram "dancer" `shouldEqual` (Right { cameraMap: (fromFoldable [(Tuple "rx" (ValueNumber 0.0)),(Tuple "ry" (ValueNumber 0.0)),(Tuple "rz" (ValueNumber 0.0)),(Tuple "x" (ValueNumber 0.0)),(Tuple "y" (ValueNumber 1.0)),(Tuple "z" (ValueNumber 10.0))]), clearMap: Nothing, dancers: [(fromFoldable [(Tuple "rx" (ValueNumber 0.0)),(Tuple "ry" (ValueNumber 0.0)),(Tuple "rz" (ValueNumber 0.0)),(Tuple "size" (ValueNumber 1.0)),(Tuple "sx" (ValueNumber 1.0)),(Tuple "sy" (ValueNumber 1.0)),(Tuple "sz" (ValueNumber 1.0)),(Tuple "x" (ValueNumber 0.0)),(Tuple "y" (ValueNumber 0.0)),(Tuple "z" (ValueNumber 0.0))])], floors: [] })
    it "parses a dancer positioned with a constant" $ parseProgram "dancer { x = 2 }" `shouldEqual` (Right { cameraMap: (fromFoldable [(Tuple "rx" (ValueNumber 0.0)),(Tuple "ry" (ValueNumber 0.0)),(Tuple "rz" (ValueNumber 0.0)),(Tuple "x" (ValueNumber 0.0)),(Tuple "y" (ValueNumber 1.0)),(Tuple "z" (ValueNumber 10.0))]), clearMap: Nothing, dancers: [(fromFoldable [(Tuple "rx" (ValueNumber 0.0)),(Tuple "ry" (ValueNumber 0.0)),(Tuple "rz" (ValueNumber 0.0)),(Tuple "size" (ValueNumber 1.0)),(Tuple "sx" (ValueNumber 1.0)),(Tuple "sy" (ValueNumber 1.0)),(Tuple "sz" (ValueNumber 1.0)),(Tuple "x" (ValueInt 2)),(Tuple "y" (ValueNumber 0.0)),(Tuple "z" (ValueNumber 0.0))])], floors: [] })
    it "parses a dancer positioned with a variable expression" $ parseProgram "dancer { x = 2.3 + osc 1.5 }" `shouldEqual` (Right { cameraMap: (fromFoldable [(Tuple "rx" (ValueNumber 0.0)),(Tuple "ry" (ValueNumber 0.0)),(Tuple "rz" (ValueNumber 0.0)),(Tuple "x" (ValueNumber 0.0)),(Tuple "y" (ValueNumber 1.0)),(Tuple "z" (ValueNumber 10.0))]), clearMap: Nothing, dancers: [(fromFoldable [(Tuple "rx" (ValueNumber 0.0)),(Tuple "ry" (ValueNumber 0.0)),(Tuple "rz" (ValueNumber 0.0)),(Tuple "size" (ValueNumber 1.0)),(Tuple "sx" (ValueNumber 1.0)),(Tuple "sy" (ValueNumber 1.0)),(Tuple "sz" (ValueNumber 1.0)),(Tuple "x" (ValueVariable (Variable.Sum (Variable.ConstantVariable 2.3) (Variable.Osc (Variable.ConstantVariable 1.5))))),(Tuple "y" (ValueNumber 0.0)),(Tuple "z" (ValueNumber 0.0))])], floors: [] })
-}
