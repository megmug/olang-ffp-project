module ParserSpec where

{- ORMOLU_DISABLE -}
{- Internal imports -}
import Parser
import Tokenizer
import SyntaxTree
{- External imports -}
import Data.Either
import Data.Maybe
import Test.Hspec
{- ORMOLU_ENABLE -}

unsafeTokenize = fromRight [] . tokenize

spec :: Spec
spec = do
  describe "parser tests" $ do
    describe "signs" $ do
      it "can parse plus sign" $
        do
          parse (unsafeTokenize "+") :: ParseResult Sign
          `shouldBe` Right Plus

      it "can parse minus sign" $
        do
          parse (unsafeTokenize "-") :: ParseResult Sign
          `shouldBe` Right Minus

    describe "operators" $ do
      it "can parse times operator" $
        do
          parse (unsafeTokenize "*") :: ParseResult Operator
          `shouldBe` Right Times

      it "can parse divide operator" $
        do
          parse (unsafeTokenize "/") :: ParseResult Operator
          `shouldBe` Right Divide

    describe "factors" $ do
      it "can parse call factor representing a simple name reference" $
        do
          parse (unsafeTokenize "test") :: ParseResult Factor
          `shouldBe` Right (CallFactor $ SymbolReference $ NameReference "test")

      it "can parse call factor representing a field reference" $
        do
          parse (unsafeTokenize "test.field") :: ParseResult Factor
          `shouldBe` Right (CallFactor $ SymbolReference $ FieldReference "test" "field")

      it "can parse class instantiation factor" $
        do
          parse (unsafeTokenize "Rational(1,2)") :: ParseResult Factor
          `shouldSatisfy` isRight

      it "can parse number factor" $
        do
          parse (unsafeTokenize "1234") :: ParseResult Factor
          `shouldBe` Right (Number 1234)

      it "can parse composite factor" $
        do
          parse (unsafeTokenize "(1)") :: ParseResult Factor
          `shouldSatisfy` isRight

    describe "terms" $ do
      it "can parse simple unit term" $
        do
          parse (unsafeTokenize "1234") :: ParseResult Term
          `shouldSatisfy` isRight

      it "can parse complex term" $
        do
          parse (unsafeTokenize "1 * 2") :: ParseResult Term
          `shouldSatisfy` isRight

    describe "expressions" $ do
      it "can parse simple unit expression without leading sign" $
        do
          parse (unsafeTokenize "1") :: ParseResult Expression
          `shouldSatisfy` isRight

      it "can parse simple unit expression with leading sign" $
        do
          parse (unsafeTokenize "+1") :: ParseResult Expression
          `shouldSatisfy` isRight

      it "can parse complex expression without leading sign" $
        do
          parse (unsafeTokenize "1 + 2 - 3") :: ParseResult Expression
          `shouldSatisfy` isRight

    describe "relations" $ do
      it "can parse equals relation" $
        do
          parse (unsafeTokenize "=") :: ParseResult Relation
          `shouldBe` Right Equals

      it "can parse smaller relation" $
        do
          parse (unsafeTokenize "<") :: ParseResult Relation
          `shouldBe` Right Smaller

      it "can parse greater relation" $
        do
          parse (unsafeTokenize ">") :: ParseResult Relation
          `shouldBe` Right Greater

    describe "conditions" $ do
      it "can parse comparison" $
        do
          parse (unsafeTokenize "1<2") :: ParseResult Condition
          `shouldSatisfy` isRight

      it "can parse negation" $
        do
          parse (unsafeTokenize "NOT 1=2") :: ParseResult Condition
          `shouldSatisfy` isRight

    describe "commands" $ do
      it "can parse assignment" $
        do
          parse (unsafeTokenize "test := 3") :: ParseResult Command
          `shouldSatisfy` isRight

      it "can parse const declaration command" $
        do
          parse (unsafeTokenize "CONST test = 3") :: ParseResult Command
          `shouldSatisfy` isRight

      it "can parse var declaration command" $
        do
          parse (unsafeTokenize "VAR test") :: ParseResult Command
          `shouldSatisfy` isRight

      it "can parse object declaration command" $
        do
          parse (unsafeTokenize "OBJ Test test") :: ParseResult Command
          `shouldSatisfy` isRight

      it "can parse call command" $
        do
          parse (unsafeTokenize "CALL test()") :: ParseResult Command
          `shouldSatisfy` isRight

      it "can parse read command" $
        do
          parse (unsafeTokenize "READ test") :: ParseResult Command
          `shouldSatisfy` isRight

      it "can parse unit command block" $
        do
          parse (unsafeTokenize "{ READ test }") :: ParseResult Command
          `shouldSatisfy` isRight

      it "can parse complex command block" $
        do
          parse (unsafeTokenize "{ READ test READ test }") :: ParseResult Command
          `shouldSatisfy` isRight

      it "can not parse empty command block" $
        do
          parse (unsafeTokenize "{ }") :: ParseResult Command
          `shouldSatisfy` isLeft

      it "can parse if-then-command" $
        do
          parse (unsafeTokenize "IF 1 = 1 THEN PRINTI test") :: ParseResult Command
          `shouldSatisfy` isRight

      it "can parse while command" $
        do
          parse (unsafeTokenize "WHILE 1 = 1 DO PRINTI test") :: ParseResult Command
          `shouldSatisfy` isRight

      it "can parse printi command" $
        do
          parse (unsafeTokenize "PRINTI test") :: ParseResult Command
          `shouldSatisfy` isRight

      it "can parse prints command" $
        do
          parse (unsafeTokenize "PRINTS \"test\"") :: ParseResult Command
          `shouldSatisfy` isRight

      it "can parse error command" $
        do
          parse (unsafeTokenize "ERROR \"test\" ") :: ParseResult Command
          `shouldSatisfy` isRight

    describe "calls" $ do
      it "can parse name reference" $
        do
          parse (unsafeTokenize "test") :: ParseResult Call
          `shouldSatisfy` isRight

      it "can parse field reference" $
        do
          parse (unsafeTokenize "test.field") :: ParseResult Call
          `shouldBe` (Right $ SymbolReference $ FieldReference "test" "field")

      it "can parse function call" $
        do
          parse (unsafeTokenize "test(1)") :: ParseResult Call
          `shouldSatisfy` isRight

      it "can parse method call" $
        do
          parse (unsafeTokenize "test.test(1)") :: ParseResult Call
          `shouldSatisfy` isRight

    describe "procedure headers" $ do
      it "can parse procedure header without return and without sub-procedures" $
        do
          parse (unsafeTokenize "test(VAR x)") :: ParseResult ProcedureHeader
          `shouldSatisfy` isRight

      it "can parse procedure header with return and without sub-procedures" $
        do
          parse (unsafeTokenize "test(VAR test) RETURNS VAR test") :: ParseResult ProcedureHeader
          `shouldSatisfy` isRight

      it "can parse procedure header with return and with one sub-procedure" $
        do
          parse (unsafeTokenize "test(VAR x) RETURNS VAR test USING [ PROCEDURE test() PRINTI 1 ]") :: ParseResult ProcedureHeader
          `shouldSatisfy` isRight

      it "can parse procedure header with return and with two sub-procedures" $
        do
          parse (unsafeTokenize "test(VAR x) RETURNS VAR test USING [ PROCEDURE test() PRINTI 1  PROCEDURE test() PRINTI 1 ]") :: ParseResult ProcedureHeader
          `shouldSatisfy` isRight

    describe "procedure declarations" $ do
      it "can parse procedure declaration" $
        do
          parse (unsafeTokenize "PROCEDURE add(VAR x, VAR y) RETURNS VAR sum { sum := a + b }") :: ParseResult ProcedureDeclaration
          `shouldSatisfy` isRight

    describe "method declarations" $ do
      it "can parse procedure declaration" $
        do
          parse (unsafeTokenize "METHOD add(VAR x, VAR y) RETURNS VAR sum { sum := a + b }") :: ParseResult MethodDeclaration
          `shouldSatisfy` isRight

    describe "field declarations" $ do
       it "can parse non-static field declaration" $
        do
          parse (unsafeTokenize "VAR x") :: ParseResult FieldDeclaration
          `shouldSatisfy` isRight

       it "can parse static field declaration" $
        do
          parse (unsafeTokenize "STATIC VAR x") :: ParseResult FieldDeclaration
          `shouldSatisfy` isRight

    describe "actual parameter lists" $ do
      it "can parse empty actual parameter list" $ do (parse (unsafeTokenize "()") :: ParseResult ActualParameterList) `shouldBe` Right []
      it "can parse singleton actual parameter list" $ do (parse (unsafeTokenize "(1)") :: ParseResult ActualParameterList) `shouldSatisfy` isRight
      it "can not parse singleton actual parameter list without closing bracket" $ do (parse (unsafeTokenize "(1") :: ParseResult ActualParameterList) `shouldSatisfy` isLeft
      it "can parse long actual parameter list" $ do (parse (unsafeTokenize "(1,2)") :: ParseResult ActualParameterList) `shouldSatisfy` isRight

    describe "formal parameter lists" $ do
      it "can parse empty formal parameter list" $ do (parse (unsafeTokenize "()") :: ParseResult FormalParameterList) `shouldBe` Right []
      it "can parse singleton formal parameter list" $ do (parse (unsafeTokenize "(VAR x)") :: ParseResult FormalParameterList) `shouldSatisfy` isRight
      it "can not parse singleton formal parameter list without closing bracket" $ do (parse (unsafeTokenize "(VAR x") :: ParseResult FormalParameterList) `shouldSatisfy` isLeft
      it "can parse long formal parameter list" $ do (parse (unsafeTokenize "(VAR x, VAR x)") :: ParseResult FormalParameterList) `shouldSatisfy` isRight

    describe "formal parameter declarations" $ do
      it "can parse formal object parameter" $
        do
          parse (unsafeTokenize "OBJ Test test") :: ParseResult FormalParameterDeclaration
          `shouldSatisfy` isRight

      it "can parse formal var parameter" $
        do
          parse (unsafeTokenize "VAR test") :: ParseResult FormalParameterDeclaration
          `shouldSatisfy` isRight

    describe "object declarations" $ do
      it "can parse object declaration" $
        do
          parse (unsafeTokenize "OBJ Test test") :: ParseResult ObjectDeclaration
          `shouldBe` Right (Object "Test" "test")

    describe "var declarations" $ do
      it "can parse var declaration" $
        do
          parse (unsafeTokenize "VAR test") :: ParseResult VarDeclaration
          `shouldBe` Right (Var "test")

    describe "const declarations" $ do
      it "can parse const declaration" $
        do
          parse (unsafeTokenize "CONST test = 3") :: ParseResult ConstDeclaration
          `shouldBe` Right (Const "test" 3)

    describe "class declarations" $ do
      it "can parse class declaration without subclass, without fields and without methods" $
        do
          parse (unsafeTokenize "CLASS Test() INIT PRINTI 1") :: ParseResult ClassDeclaration
          `shouldSatisfy` isRight

      it "can parse class declaration without subclass, without fields and with one method" $
        do
          parse (unsafeTokenize "CLASS Test() INIT PRINTI 1 [ METHOD test() PRINTI 1 ]") :: ParseResult ClassDeclaration
          `shouldSatisfy` isRight

      it "can parse class declaration without subclass, one field and one method" $
        do
          parse (unsafeTokenize "CLASS Test() FIELDS VAR x INIT PRINTI 1 [ METHOD test() PRINTI 1 ]") :: ParseResult ClassDeclaration
          `shouldSatisfy` isRight

      it "can parse class declaration with subclass, one field and one method" $
        do
          parse (unsafeTokenize "CLASS Test() SUBCLASSOF Test FIELDS VAR x INIT PRINTI 1 [ METHOD test() PRINTI 1 ]") :: ParseResult ClassDeclaration
          `shouldSatisfy` isRight

      it "can parse class declaration with subclass, two fields and one method" $
        do
          parse (unsafeTokenize "CLASS Test() SUBCLASSOF Test FIELDS VAR x VAR x INIT PRINTI 1 [ METHOD test() PRINTI 1 ]") :: ParseResult ClassDeclaration
          `shouldSatisfy` isRight
      
      it "can parse class declaration with subclass, two fields and two methods" $
        do
          parse (unsafeTokenize "CLASS Test() SUBCLASSOF Test FIELDS VAR x VAR x INIT PRINTI 1 [ METHOD test() PRINTI 1  METHOD test() PRINTI 1 ]") :: ParseResult ClassDeclaration
          `shouldSatisfy` isRight

      it "can not parse class declaration without subclass, no field and one method" $
        do
          parse (unsafeTokenize "CLASS Test() FIELDS INIT PRINTI 1 [ METHOD test() PRINTI 1 ]") :: ParseResult ClassDeclaration
          `shouldSatisfy` isLeft

      it "can not parse class declaration without subclass, one field and no method" $
        do
          parse (unsafeTokenize "CLASS Test() FIELDS VAR x INIT PRINTI 1 [ ]") :: ParseResult ClassDeclaration
          `shouldSatisfy` isLeft

    describe "programs" $ do
      it "can parse program without class or procedure declarations" $
        do
          parse (unsafeTokenize "PROGRAM hello DO {VAR x x := 1}") :: ParseResult Program
          `shouldSatisfy` isRight

      it "can parse program without class or procedure declarations, but with USING block" $
        do
          parse (unsafeTokenize "PROGRAM hello USING [ ] DO {VAR x x := 1}") :: ParseResult Program
          `shouldSatisfy` isRight
      
      it "can parse program with one class and no procedure declarations" $
        do
          parse (unsafeTokenize "PROGRAM hello USING [ CLASS Test() FIELDS VAR x INIT PRINTI 1 [ METHOD test() PRINTI 1 ] ] DO {VAR x x := 1}") :: ParseResult Program
          `shouldSatisfy` isRight

      it "can parse program with no class and one procedure declaration" $
        do
          parse (unsafeTokenize "PROGRAM hello USING [ PROCEDURE add(VAR x, VAR y) RETURNS VAR sum { sum := a + b } ] DO {VAR x x := 1}") :: ParseResult Program
          `shouldSatisfy` isRight

      it "can parse program with one class and one procedure declaration" $
        do
          parse (unsafeTokenize "PROGRAM hello USING [ CLASS Test() FIELDS VAR x INIT PRINTI 1 [ METHOD test() PRINTI 1 ] PROCEDURE add(VAR x, VAR y) RETURNS VAR sum { sum := a + b } ] DO {VAR x x := 1}") :: ParseResult Program
          `shouldSatisfy` isRight

      it "can parse program with two classes and one procedure declaration" $
        do
          parse (unsafeTokenize "PROGRAM hello USING [ CLASS Test() FIELDS VAR x INIT PRINTI 1 [ METHOD test() PRINTI 1 ]  CLASS Test() FIELDS VAR x INIT PRINTI 1 [ METHOD test() PRINTI 1 ]  PROCEDURE add(VAR x, VAR y) RETURNS VAR sum { sum := a + b } ] DO {VAR x x := 1}") :: ParseResult Program
          `shouldSatisfy` isRight

      it "can parse program with two classes and two procedure declarations" $
        do
          parse (unsafeTokenize "PROGRAM hello USING [ CLASS Test() FIELDS VAR x INIT PRINTI 1 [ METHOD test() PRINTI 1 ]  CLASS Test() FIELDS VAR x INIT PRINTI 1 [ METHOD test() PRINTI 1 ]  PROCEDURE add(VAR x, VAR y) RETURNS VAR sum { sum := a + b }  PROCEDURE add(VAR x, VAR y) RETURNS VAR sum { sum := a + b } ] DO {VAR x x := 1}") :: ParseResult Program
          `shouldSatisfy` isRight