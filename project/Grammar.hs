module Grammar where

import Data.List (nub, sortOn, groupBy)

type Variable = String
type Production = [ProdExpr]
type ProductionRules = (Variable, [Production])

-- grammar defined by the start variable and the list of production rules for each variable
data Grammar = Grammar Variable [ProductionRules]
  deriving Show

-- outer lists ~ expressions separated by '|' (disjunction)
-- inner lists ~ juxtaposed expressions (concatenation)
data ProdExpr = 
    Grp       [[ProdExpr]]
  | Mayb      [[ProdExpr]]
  | Seq       [[ProdExpr]]
  | Var       Variable
  | Terminals String
    deriving Show

startVariable :: Grammar -> Variable
startVariable (Grammar v _) = v

rulesMap :: Grammar -> [ProductionRules]
rulesMap (Grammar _ rs) = rs

variables :: Grammar -> [Variable]
variables (Grammar _ m) = nub $ map rulesVariable m

rulesVariable :: ProductionRules -> Variable
rulesVariable = fst

rulesProductions :: ProductionRules -> [Production]
rulesProductions = snd

replaceVar :: Grammar -> Variable -> [Production]
replaceVar g v = unsafeLookup v (rulesMap g)
  where unsafeLookup x xys = case lookup x xys of
          Nothing -> error "Unsafe lookup failed"
          Just y  -> y

-- takes parsed production rules and returns Either an error message or Grammar object after running
-- "cleanup" on the production rules and extracting the start variable
mkGrammar :: [ProductionRules] -> Either String Grammar
mkGrammar [] = Left "Error. Empty set of production rules."
mkGrammar xs = let startVar = rulesVariable (head xs)
               in (pure (combineRules xs)) >>= validateProductionVars >>= \rules -> pure $ Grammar startVar rules

-- If there are two+ production lines with the same producing variable name,
-- then combine the contents of the lists in the two lines
combineRules :: [ProductionRules] -> [ProductionRules]
combineRules g = f $ groupBy (\x y -> rulesVariable x == rulesVariable y) (sortOn rulesVariable g)
  where f = map (\ys -> ((rulesVariable . head) ys, foldr (++) [] (map rulesProductions ys)))

-- Ensures that each referenced variable in productions are variables
-- with valid production rules in the grammar
-- Returns Right  if valid, or Left (variable name) if invalid
validateProductionVars :: [ProductionRules] -> Either String [ProductionRules]
validateProductionVars g = sequence_ (map f (map snd g)) >> pure g 
  where f :: [[ProdExpr]] -> Either String ()
        f = sequence_ . map h
        h :: [ProdExpr] -> Either String ()
        h = sequence_ . map r
        r :: ProdExpr -> Either String ()
        r e = case e of
          Terminals _ -> Right ()
          Var v       -> if v `elem` (map rulesVariable g) 
                         then Right ()
                         else Left v
          Grp exprs   -> f exprs
          Mayb exprs  -> f exprs
          Seq exprs   -> f exprs

gpagrammarstr = "<gpa> ::= '4.0'['0']\n<gpa> ::= <first-dig> '.' <decimal> [<decimal>]\n<first-dig> ::= '0'|'1'|'2'|'3'\n<decimal> ::= <first-dig> |'4'|'5'|'6'|'7'|'8'|'9'"
