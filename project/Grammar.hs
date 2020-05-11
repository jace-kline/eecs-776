module Grammar where

type Variable = String

type Production = [ProdExpr]

data ProdExpr = 
    Prim ProdComp
  | Mayb ProdExpr
  | Seq ProdExpr
  | Opts [ProdExpr]
    deriving Show

data ProdComp = 
    Var Variable 
  | Terminals String
    deriving Show

               
