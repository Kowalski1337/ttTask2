module Main where

import Proof
import Typification
import Lyambda
import Lexer (alexScanTokens)
import Parser (parseExpr)


help :: [String] -> IO ()
help (s : xs)  = do
  putStrLn s
  help xs
help _ = do
  putStr ""

main :: IO ()
main = do
  input <- getContents
  case parseExpr (alexScanTokens input) of
    Left err   -> putStrLn err
    Right expr -> case (getProof expr) of
      Just proof -> help (showMeProof proof "")
      Nothing -> putStrLn "Expression has no type"
