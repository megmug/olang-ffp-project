module TokenizerSpec where

import Test.Hspec
import Token
import Tokenizer
import Data.Either

spec :: Spec
spec = do
  describe "basic tests" $ do
    it "can tokenize simple assignment command" $ do
      tokenize "hello := 1 + 1" `shouldSatisfy` isRight

    it "can tokenize all zero-argument tokens seperated by spaces" $ do
      tokenize "PROGRAM USING CLASS SUBCLASSOF FIELDS INIT OBJ STATIC CONST VAR PROCEDURE METHOD RETURNS CALL READ WRITE IF THEN WHILE DO ERROR NOT := = , . > < + - * / ( ) [ ] { }"
        `shouldSatisfy` isRight

    it "can tokenize simple program" $ do
      tokenize "PROGRAM hello DO {VAR x x := 1}" `shouldSatisfy` isRight
