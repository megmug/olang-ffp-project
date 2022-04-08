module CodeGeneratorSpec where

import CodeGenerator
{- External imports -}

import Command
import Data.Either
import Data.Maybe
import Parser
import SyntaxTree
import Test.Hspec
import qualified Text.Parsec as Text.Parsec.Error
import Tokenizer

unsafeParseCommand :: String -> SyntaxTree.Command
unsafeParseCommand = fromRight (SyntaxTree.Read "") . parse . fromRight [] . tokenize

unsafeParseProgram :: String -> Program
unsafeParseProgram = fromRight (Program [] [] (SyntaxTree.Read "")) . parse . fromRight [] . tokenize

unsafeGenerate :: String -> Either String [Command.Command]
unsafeGenerate = generate . unsafeParseProgram

spec :: Spec
spec = do
  describe "helper function tests" $ do
    describe "commands" $ do
      it "can count simple command mem requirements correctly" $
        do
          calculateCommandStackMemoryRequirements
            ( unsafeParseCommand
                "{ VAR m \
                \ m := 2 \
                \ isprime := 1 \
                \ WHILE m < n DO { \
                \ IF (n / m) * m = n THEN isprime := 0 \
                \ m := m + 1 \
                \ } }" ::
                SyntaxTree.Command
            )
            `shouldBe` 1
          calculateCommandStackMemoryRequirements
            ( unsafeParseCommand
                "{ VAR m \
                \ VAR n }" ::
                SyntaxTree.Command
            )
            `shouldBe` 2
          calculateCommandStackMemoryRequirements
            ( unsafeParseCommand
                "{ }" ::
                SyntaxTree.Command
            )
            `shouldBe` 0
      it "can calculate complicated command mem requirements correctly" $
        do
          calculateCommandStackMemoryRequirements (unsafeParseCommand "{VAR m {VAR n}}") `shouldBe` 2
          calculateCommandStackMemoryRequirements (unsafeParseCommand "{{VAR n} VAR m}") `shouldBe` 1
          calculateCommandStackMemoryRequirements (unsafeParseCommand "{VAR m {VAR n} VAR l}") `shouldBe` 2
          calculateCommandStackMemoryRequirements (unsafeParseCommand "{VAR m {VAR n VAR k} VAR l}") `shouldBe` 3

  describe "compiling test programs" $ do
    it "can not compile illegalreference program" $ do
      prog <- readFile "resources/test-programs/illegalreference.olang"
      unsafeGenerate prog `shouldSatisfy` isLeft

    it "can compile divzero program" $ do
      prog <- readFile "resources/test-programs/divzero.olang"
      unsafeGenerate prog `shouldSatisfy` isRight

  describe "compiling example programs" $ do
    it "can compile ackermann program" $ do
      prog <- readFile "resources/example-programs/ackermann.olang"
      unsafeGenerate prog `shouldSatisfy` isRight

    it "can compile animals program" $ do
      prog <- readFile "resources/example-programs/animals.olang"
      unsafeGenerate prog `shouldSatisfy` isRight

    it "can compile bank program" $ do
      prog <- readFile "resources/example-programs/bank.olang"
      unsafeGenerate prog `shouldSatisfy` isRight

    it "can compile linked list program" $ do
      prog <- readFile "resources/example-programs/linkedlist.olang"
      unsafeGenerate prog `shouldSatisfy` isRight

    it "can compile potfak program" $ do
      prog <- readFile "resources/example-programs/potfak.olang"
      unsafeGenerate prog `shouldSatisfy` isRight

    it "can compile primes program" $ do
      prog <- readFile "resources/example-programs/primes.olang"
      unsafeGenerate prog `shouldSatisfy` isRight

    it "can compile rational program" $ do
      prog <- readFile "resources/example-programs/rational.olang"
      unsafeGenerate prog `shouldSatisfy` isRight
