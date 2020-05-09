module Grammar where

type Variable = String

data ProdExpr = 
    Prim ProdComponent
  | Mayb ProdExpr
  | Seq ProdExpr
  | Opts [ProdExpr]

data ProdComp = Var Variable | Terminals String
               
