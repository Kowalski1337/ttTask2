module Type where

data CarryType = VarType String |
                 Impl CarryType CarryType
                 deriving (Eq)

instance Show CarryType where
  show (VarType s) = s
  show (Impl f s) = "(" ++ show f ++ "->" ++ show s ++ ")"
