import Parser
import SyntaxTree
import System.Environment
import System.IO
import Tokenizer
import CodeGenerator
import Machine

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  args <- getArgs
  input <- readFile (head args)
  case tokenize input of
    Left e -> putStrLn $ "Lexical error: " ++ show e
    Right t -> do
      case parse t :: ParseResult Program of
        Left e -> putStrLn ("Parse error: " ++ show e)
        Right pro -> do
          case generate pro of
            Left e -> putStrLn $ "Compilation error: " ++ e
            Right cmds -> do
              run cmds
