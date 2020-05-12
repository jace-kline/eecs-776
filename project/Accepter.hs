module Accepter where
-- The purpose of this module is to define the functionality
-- for our backtracking accepter automaton.
-- Given a grammar and an input string, we want to determine
-- whether or not the string can be generated by the grammar.
-- Obviously, the complexity of such a task has exponential
-- complexity based on input size and grammar size, and simplifications
-- are very difficult given that the grammar itself is a variable,
-- and can be any context-free grammar.

import Grammar
import Parser
import ParseTree
import Control.Applicative

parseMatch :: Grammar -> Parser Char ParseTree
parseMatch g = parseMatch' $ Var $ startVariable g
    where 
        parseMatch' :: ProdExpr -> Parser Char ParseTree
        parseMatch' e = case e of
            (Terminals s) -> Parser $ \w ->
                if take (length s) w == s
                then Right (Leaf s, drop (length s) w)
                else Left []
            (Var v)   -> fmap (VarReplace v) $ parseDisjunction (replaceVar g v)
            (Grp rs)  -> fmap GrpNode $ parseDisjunction rs
            (Mayb rs) -> fmap MaybNode $ (parseDisjunction rs) <|> pure Nil
            (Seq rs)  -> fmap SeqNode $ ((fmap ConcatNode (some (parseDisjunction rs)))) <|> pure Nil
        parseDisjunction :: [[ProdExpr]] -> Parser Char ParseTree
        parseDisjunction rs = fmap ConcatNode $ appSum $ map parseConjunction rs
        parseConjunction :: [ProdExpr] -> Parser Char [ParseTree]
        parseConjunction = sequence . fmap parseMatch'