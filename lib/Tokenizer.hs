{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use $>" #-}

module Tokenizer (tokenize) where

import Text.Parsec
import Text.Parsec.Prim
import Token

-- we have tokenizers for single tokens
type UnitTokenizer = Parsec String () TokenPos

-- a tokenizer for tokenizing a whole stream
type Tokenizer = Parsec String () [TokenPos]

-- tokenizers that ignore certain chars
type Ignorer = Parsec String () Char

getUnitTokenizer :: String -> Token -> UnitTokenizer
getUnitTokenizer s t = ((,) t <$> getPosition) <* string s

getUnitTokenizers :: [(String, Token)] -> [UnitTokenizer]
getUnitTokenizers [] = []
getUnitTokenizers ((s, t) : sts) = getUnitTokenizer s t : getUnitTokenizers sts

-- here we define the mapping from our basic string tokens to their atomic abstract token counterparts
unittokenizers :: [UnitTokenizer]
unittokenizers =
  getUnitTokenizers
    [ ("PROGRAM", PROGRAM),
      ("USING", USING),
      ("CLASS", CLASS),
      ("SUBCLASSOF", SUBCLASSOF),
      ("FIELDS", FIELDS),
      ("INIT", INIT),
      ("OBJ", OBJ),
      ("STATIC", STATIC),
      ("CONST", CONST),
      ("VAR", VAR),
      ("PROCEDURE", PROCEDURE),
      ("METHOD", METHOD),
      ("RETURNS", RETURNS),
      ("CALL", CALL),
      ("READ", READ),
      ("PRINTI", PRINTI),
      ("PRINTS", PRINTS),
      ("PRINTLNS", PRINTLNS),
      ("IF", IF),
      ("THEN", THEN),
      ("WHILE", WHILE),
      ("DO", DO),
      ("ERROR", ERROR),
      ("NOT", NOT),
      (":=", (::=)),
      ("=", (:=)),
      (",", Comma),
      (".", (:.)),
      (">", (:>)),
      ("<", (:<)),
      ("+", (:+)),
      ("-", (:-)),
      ("*", (:*)),
      ("/", (:/)),
      ("(", OpenRoundBracket),
      (")", CloseRoundBracket),
      ("[", OpenSquareBracket),
      ("]", CloseSquareBracket),
      ("{", OpenCurlyBracket),
      ("}", CloseCurlyBracket)
    ]

lowerchar = ['a' .. 'z']

upperchar = ['A' .. 'Z']

-- here we define a few tokenizers for parsing non-atomic tokens
name :: UnitTokenizer
name = do
  p <- getPosition
  h <- satisfy (`elem` lowerchar)
  n <- many $ satisfy (`elem` lowerchar ++ upperchar)
  return (Name $ h : n, p)

classname :: UnitTokenizer
classname = do
  p <- getPosition
  h <- satisfy (`elem` upperchar)
  r <- many $ satisfy (`elem` lowerchar ++ upperchar)
  return (ClassName $ h : r, p)

tString :: UnitTokenizer
tString = do
  p <- getPosition
  s <- char '"' *> many (satisfy (/= '"')) <* char '"'
  return (String s, p)

number :: UnitTokenizer
number = do
  p <- getPosition
  ds <- many1 digit
  return (Number $ read ds, p)

someToken :: UnitTokenizer
someToken = choice $ try <$> unittokenizers ++ [name, classname, tString, number]

anyIgnored :: Ignorer
anyIgnored = choice [space, endOfLine, tab]

-- this is our program tokenizer
tokenizer :: Tokenizer
tokenizer = do
  ts <- many $ try (many anyIgnored *> someToken)
  many anyIgnored
  eof
  return ts

tokenize :: String -> Either ParseError [TokenPos]
tokenize = Text.Parsec.parse tokenizer "input"