{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
module SyntaxTree where

import Data.List.NonEmpty


{- There is a type for all important nonterminals in the grammar 
 - The grammar is not represented 1:1, rather an optimized version that is stripped of unnecessary details
 - This makes it easier to compile the language
 -}
type ClassName = String

type Name = String

type Number = Integer

type IsStatic = Bool

type Initializer = Command

type ReturnParameterDeclaration = FormalParameterDeclaration

type ActualParameterList = [Expression]

type FormalParameterList = [FormalParameterDeclaration]

data Program
  = Program
      [ClassDeclaration]
      [ProcedureDeclaration]
      Command
  deriving (Eq, Show)

data ClassDeclaration
  = Class
      ClassName
      FormalParameterList
      (Maybe ClassName)
      [FieldDeclaration]
      Initializer
      [MethodDeclaration]
  deriving (Eq, Show)

data ConstDeclaration = Const Name Number deriving (Eq, Show)

data VarDeclaration = Var Name deriving (Eq, Show)

data ObjectDeclaration = Object ClassName Name deriving (Eq, Show)

data FormalParameterDeclaration
  = VarParameter VarDeclaration
  | ObjectParameter ObjectDeclaration
  deriving (Eq, Show)

data FieldDeclaration = Field IsStatic FormalParameterDeclaration deriving (Eq, Show)

data MethodDeclaration = Method ProcedureHeader Command deriving (Eq, Show)

data ProcedureDeclaration = Procedure ProcedureHeader Command deriving (Eq, Show)

data ProcedureHeader
  = ProcedureHeader
      Name
      FormalParameterList
      (Maybe ReturnParameterDeclaration)
      [ProcedureDeclaration]
  deriving (Eq, Show)

data Call
  = SymbolReference SymbolReference
  | Call SymbolReference ActualParameterList
  deriving (Eq, Show)

data SymbolReference
  = NameReference Name
  | FieldReference Name Name
  deriving (Eq, Show)

data Command
  = Assignment SymbolReference Expression
  | ConstDeclarationCommand ConstDeclaration
  | VarDeclarationCommand VarDeclaration
  | ObjectDeclarationCommand ObjectDeclaration
  | CallCommand Call
  | Read Name
  | Block (NonEmpty Command)
  | IfThen Condition Command
  | While Condition Command
  | PrintI Expression
  | PrintS String
  | PrintLnS String
  | Error
  deriving (Eq, Show)

data Condition
  = Comparison Expression Relation Expression
  | Negation Condition
  deriving (Eq, Show)

data Relation = Equals | Smaller | Greater deriving (Eq, Show)

data Expression
  = Expression
      (NonEmpty (Sign, Term))
  deriving (Eq, Show)

data Term
  = Term
      Factor
      [(Operator, Factor)]
  deriving (Eq, Show)

data Factor
  = CallFactor Call
  | ClassInstantiation ClassName ActualParameterList
  | Number Number
  | CompositeFactor Expression
  deriving (Eq, Show)

data Sign = Plus | Minus deriving (Eq, Show)

data Operator = Times | Divide deriving (Eq, Show)