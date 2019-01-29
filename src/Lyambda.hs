module Lyambda where

data Expression = Var String | Ap Expression Expression | Ab Expression Expression
  deriving (Eq)

instance Show Expression where
  show (Var s) = s
  show (Ap f s) = "(" ++ show f ++ " " ++ show s ++ ")"
  show (Ab f s) = "(\\" ++ show f ++ "." ++ show s ++ ")"


-- example :: Expression
-- example = Ab "a" (Ab "b" (Ap (Ap (Ap (Ap (Var "a") (Var "b"))(Var "c")) (Ab "d" (Ap (Var "e") (Ab "f" (Var "g")))))(Var "h")))
