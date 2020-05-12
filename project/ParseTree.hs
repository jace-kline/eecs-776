module ParseTree where

data ParseTree = Nil 
               | Leaf String
               | ConcatNode [ParseTree]
               | GrpNode  ParseTree
               | MaybNode ParseTree
               | SeqNode  ParseTree 
               | VarReplace String ParseTree
                    deriving Show

-- instance Show ParseTree where ...

