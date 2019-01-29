module Proof where

import Type
import Lyambda
import Data.List

data TypeTree = Zero CarryType | One CarryType TypeTree | Two CarryType TypeTree TypeTree
  deriving (Show)

data Line = L [(String, CarryType)] Expression CarryType Int

instance Show Line where
  show (L context expr tp num) =
    let
      (a, b) = unzip context
    in
      if
        (length a == 0)
      then
        "|- " ++ (exprWithType (show expr) tp) ++ " [rule #" ++ (show num) ++ "]"
      else
        (intercalate ", " (zipWith ($) (map exprWithType a) b)) ++ " |- " ++ (exprWithType (show expr) tp) ++ " [rule #" ++ (show num) ++ "]"

test1 = L [("1", (Impl (VarType "a") (VarType "a"))), ("y", (Impl (VarType "b") (VarType "c")))] (Ab (Var "x") (Var "x")) (Impl (VarType "a") (VarType "c")) 3
test2 = L [("2", (Impl (VarType "a") (VarType "a"))), ("y", (Impl (VarType "b") (VarType "c")))] (Ab (Var "x") (Var "x")) (Impl (VarType "a") (VarType "c")) 3
test3 = L [("3", (Impl (VarType "a") (VarType "a"))), ("y", (Impl (VarType "b") (VarType "c")))] (Ab (Var "x") (Var "x")) (Impl (VarType "a") (VarType "c")) 3
test4 = L [("4", (Impl (VarType "a") (VarType "a"))), ("y", (Impl (VarType "b") (VarType "c")))] (Ab (Var "x") (Var "x")) (Impl (VarType "a") (VarType "c")) 3
test5 = L [("5", (Impl (VarType "a") (VarType "a"))), ("y", (Impl (VarType "b") (VarType "c")))] (Ab (Var "x") (Var "x")) (Impl (VarType "a") (VarType "c")) 3
test6 = L [("6", (Impl (VarType "a") (VarType "a"))), ("y", (Impl (VarType "b") (VarType "c")))] (Ab (Var "x") (Var "x")) (Impl (VarType "a") (VarType "c")) 3

exprWithType :: String -> CarryType -> String
exprWithType var t = var ++ " : " ++ (show t)

data ProofType = Leaf Line | App Line ProofType ProofType | Abs Line ProofType
  deriving (Show)

testProof = (App test1 (App test6 (Abs test3 (Leaf test4)) (Leaf test5)) (Leaf test2))

simpleTest = (Abs test1 (Leaf test2))

lol = (Leaf test1)

showMeProof :: ProofType -> String -> [String]
showMeProof (Leaf line) pref = [pref ++ (show line)]
showMeProof (App line f s) pref = (pref ++ (show line)) : (showMeProof f (pref ++ "*   ")) ++ (showMeProof s (pref ++ "*   "))
showMeProof (Abs line next) pref = (pref ++ (show line)) : (showMeProof next (pref ++ "*   "))
