module ParseTree where

import Grid

data ParseTree = Nil 
               | Leaf String
               | ConcatNode [ParseTree]
               | GrpNode  ParseTree
               | MaybNode ParseTree
               | SeqNode  ParseTree 
               | VarReplace String ParseTree
                    deriving Show

derivedString :: ParseTree -> String
derivedString t = 
     case t of
          Nil           -> ""
          Leaf s        -> s
          GrpNode t'    -> derivedString t'
          MaybNode t'   -> derivedString t'
          SeqNode t'    -> derivedString t'
          VarReplace v t' -> derivedString t'
          ConcatNode ts -> foldr (++) [] $ map derivedString ts

toShowGrid :: ParseTree -> Grid Char
toShowGrid t = go t
     where 
          go t = case t of
                    Nil         -> addOrigin "Nil"
                    Leaf s      -> addOrigin (show s)
                    GrpNode t'  -> go t'
                    MaybNode t' -> f "?" t'
                    SeqNode t'  -> f "*" t'
                    VarReplace v t' -> f ("<" ++ v ++ ">") t'
                    ConcatNode ts -> 
                         let subgrids = map go ts
                             (w, grid) = foldl (\(c, grd) subgrd -> (c + width subgrd + 2, mergeGrids 3 c subgrd (addListV grd 1 c "||"))) (0, g) subgrids
                         in addListH grid 0 0 $ take (w - (width (last subgrids)) - 1) $ repeat '-'
          g = (mkGrid :: Grid Char)
          addOrigin = addListH g 0 0
          f str t' = mergeGrids 3 0 (toShowGrid t') $ addListV (addOrigin str) 1 0 "||"

-- instance Show ParseTree where
--      show t = foldr (++) [] $ map f $ gridLists $ toShowGrid t
--           where
--                f m_xs = case m_xs of
--                     Nothing -> "\n"
--                     Just xs -> map g xs
--                g m_x = case m_x of
--                     Nothing -> ' '
--                     Just x  -> x

