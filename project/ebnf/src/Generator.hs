module Generator where

import Grammar
import ParseTree
import System.Random

max_seq_length :: Int
max_seq_length = 5

generateTree :: Grammar -> StdGen -> ParseTree
generateTree g gen = genNode gen $ Var (startVariable g)
    where 
        genNode :: StdGen -> ProdExpr -> ParseTree
        genNode gen e = case e of
            (Terminals s) -> Leaf s
            (Var v)       -> VarReplace v $ genNode gen $ Grp $ replaceVar g v
            (Grp rs)      -> subTree gen rs
            (Mayb rs)     -> MaybNode $ if mayb then subTree gen rs else Nil
            (Seq rs)      -> SeqNode $ f rs
        mayb = fst (random gen :: (Bool, StdGen))
        subTree gn rs = ConcatNode $ subs gn $ rs !! chooseDisjunctionIndex rs
        subs gn xs = 
            let gens = (generators gn (length xs))
            in map (\(gn, x) -> genNode gn x) $ zip gens xs
        chooseDisjunctionIndex rs = fst (randomR (0, length rs - 1) gen :: (Int, StdGen))
        f rs = if len == 0 
               then Nil 
               else ConcatNode $ map (\gn -> subTree gn rs) (generators gen len)
        len = fst (randomR (0, max_seq_length) gen :: (Int, StdGen))

generators :: StdGen -> Int -> [StdGen]
generators g n = go g n
    where go g 1 = [fst (split g)]
          go g n = 
              let rest = go g (n - 1)
                  (_, g_new) = next $ head rest
              in g_new : rest
