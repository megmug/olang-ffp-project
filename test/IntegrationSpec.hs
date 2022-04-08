module IntegrationSpec where

import CodeGenerator
{- External imports -}

import Command
import Data.Either
import Data.Maybe
import Machine
import Parser
import SyntaxTree
import Test.Hspec
import qualified Text.Parsec as Text.Parsec.Error
import Tokenizer
import Control.Exception (evaluate)
import Control.DeepSeq(force)

unsafeParseProgram :: String -> Program
unsafeParseProgram = fromRight (Program [] [] (SyntaxTree.Read "")) . parse . fromRight [] . tokenize

unsafeGenerate :: String -> [Command.Command]
unsafeGenerate p = fromRight [] $ generate $ unsafeParseProgram p

spec :: Spec
spec = do
    describe "running programs" $ do
        it "test ackermann program outputs the right numbers" $ do
            ackSource <- readFile "resources/test-programs/ackermann.olang"
            let ackProgram = unsafeGenerate ackSource
            let ackOutput = runTest ackProgram ["3", "3"]
            ackOutput `shouldSatisfy` isRight
            case ackOutput of
              Left err -> return ()
              Right out -> out `shouldBe` "61\nProgram terminated successfully.\n"

            let sndAckOutput = runTest ackProgram ["3", "4"]
            sndAckOutput `shouldSatisfy` isRight
            case sndAckOutput of
              Left err -> return ()
              Right out -> out `shouldBe` "125\nProgram terminated successfully.\n"

        it "dividing by zero throws an exception" $ do
            divSource <- readFile "resources/test-programs/divzero.olang"
            let divProgram = unsafeGenerate divSource
            (evaluate . force) (runTest divProgram []) `shouldThrow` anyArithException
            
