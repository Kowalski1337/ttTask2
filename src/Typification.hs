module Typification where

import Proof
import Lyambda
import Type
import qualified Data.Map as M
import qualified Data.List as L

-- \f.(\x. f (x x)) (\x. f (x x))


sample :: Expression
sample = Ab (Var "f") (Ap (Ab (Var "x")(Ap (Var "f") (Ap (Var "x")(Var "x"))))  (Ab (Var "x")(Ap (Var "f") (Ap (Var "x")(Var "x")))) )

simple :: Expression
simple = Ap (Ab (Var "x")(Ap (Var "x")(Var "x"))) (Ab (Var "x")(Ap (Var "x")(Var "x")))

generateSystem :: M.Map String CarryType -> Expression -> Int -> (M.Map String CarryType, CarryType, [(CarryType, CarryType)], Int, TypeTree)
generateSystem mp (Var s) num =
    case (M.lookup s mp) of
        Just t -> (mp, t, [], num, (Zero t))
        Nothing ->
          let
            newType = getVar num
          in
            (M.insert s newType mp, newType, [], num + 1, (Zero newType))
          where
            getVar :: Int -> CarryType
            getVar num = VarType ('t' : (show num))
generateSystem mp (Ap l r) num =
  let
    (mp1, t1, lSyst, lnum, tree1) = generateSystem mp l num
    (mp2, t2, rSyst, rnum, tree2) = generateSystem mp1 r lnum
    newType = getVar rnum
  in
    (M.union mp1 mp2, newType, (t1, (Impl t2 newType)) : lSyst ++ rSyst, (rnum + 1), (Two newType tree1 tree2))
  where
    getVar :: Int -> CarryType
    getVar num = VarType ('t' : (show num))

generateSystem mp (Ab (Var s) expr) num =
  let
    varType = (getVar num)
    (mpp, t, exprSyst, nnum, tree) = generateSystem (M.insert s varType mp) expr (num + 1)
    newType = Impl varType t
  in
    (M.delete s mpp, newType, exprSyst, (nnum+1), (One newType tree))
  where
    getVar :: Int -> CarryType
    getVar num = VarType ('t' : (show num))


substitute :: CarryType -> CarryType -> (Bool, (CarryType, CarryType)) ->  (Bool, (CarryType, CarryType))
substitute x expr (b, (first, second))  = (b, ((substituteOne x expr first), (substituteOne x expr second)))

substituteOne :: CarryType -> CarryType -> CarryType -> CarryType
substituteOne x expr (Impl f s) = Impl (substituteOne x expr f) (substituteOne x expr s)
substituteOne x expr var        =
    if (var == x)
    then expr
    else var


expr1 = Impl (VarType "x") (Impl (VarType "y")(VarType "z"))
expr2 = Impl (Impl (VarType "x") (VarType "y")) (Impl (VarType "y")(VarType "z"))
expr3 = Impl (VarType "c") (VarType "c")

addCheckers :: [(CarryType, CarryType)] -> [(Bool, (CarryType, CarryType))]
addCheckers (x : xs) = (False, x) : (addCheckers xs)
addCheckers _ = []

solveSystem :: [(Bool, (CarryType, CarryType))] -> Maybe [(Bool, (CarryType, CarryType))]
solveSystem syst@((True, _) : _)       = Just syst
solveSystem syst@((_, (f, s)) : other) =
    if f == s
    then solveSystem other
    else solve syst
      where
        solve :: [(Bool, (CarryType, CarryType))] -> Maybe [(Bool, (CarryType, CarryType))]
        solve ((_, ((Impl l1 l2), (Impl r1 r2))) : other) =
             solveSystem ((False, (l1, r1)) : (False, (l2, r2)) : other)
        solve ((_, eq@(w@(VarType x), expr)) : other)     =
            if contains w expr
            then Nothing
            else solveSystem ((map (substitute w expr) other) ++ [(True, eq)])
              where
                contains :: CarryType -> CarryType -> Bool
                contains x (Impl f s) = (contains x f) || (contains x s)
                contains x y = x == y
        solve ((_, (expr, w@(VarType x))) : other)        = solveSystem ((False, (w,expr)) : other)
        solve _                                           = Just []
solveSystem _                          = Just []


generateProof :: M.Map String CarryType -> TypeTree -> Expression -> ProofType
generateProof mp (Zero t) expr                             = Leaf (L (M.toList mp) expr t 1)
generateProof mp (Two t f s) expr@(Ap l r)                 = App (L (M.toList mp) expr t 2) (generateProof mp f l) (generateProof mp s r)
generateProof mp (One t@(Impl tt _) e) expr@(Ab (Var s) r) = Abs (L (M.toList mp) expr t 3) (generateProof (M.insert s tt mp) e r)


multySubs :: [CarryType -> CarryType] -> CarryType -> CarryType
multySubs (x : xs) w = x (multySubs xs w)
multySubs [] w = w

mmultySubs :: [CarryType -> CarryType] -> [CarryType] -> [CarryType]
mmultySubs (x : xs) w =map x (mmultySubs xs w)
mmultySubs [] w = w

subsToTree :: [CarryType -> CarryType] -> TypeTree -> TypeTree
subsToTree funcs (Zero t) = (Zero (multySubs funcs t))
subsToTree funcs (One t next) = (One (multySubs funcs t) (subsToTree funcs next))
subsToTree funcs (Two t l r) = (Two (multySubs funcs t) (subsToTree funcs l) (subsToTree funcs r))


getProof :: Expression -> Maybe ProofType
getProof expr =
  let
    (a,b,c,d,e) = generateSystem M.empty expr 0
  in
    case (solveSystem (addCheckers c)) of
        Just s ->
          let
            listSubs = snd $ unzip s
            listWhere = M.toList a
            listFoo = (zipWith ($) (map substituteOne (fst $ unzip listSubs)) (snd $ unzip listSubs))
            proof = generateProof (M.fromList $ zip (fst $ unzip listWhere) (mmultySubs listFoo (snd $ unzip listWhere))) (subsToTree listFoo e) expr
          in
            Just proof

        Nothing -> Nothing
