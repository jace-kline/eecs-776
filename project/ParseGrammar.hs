module ParseGrammar where

import Parser
import Grammar
import Data.Char (isSpace, isLetter, isAlphaNum, isPrint)
import Control.Applicative

-- used for stripping a token that doesn't add semantic value, but still important to syntax rules
-- partially apply with a token, and can be wrapped into an instance of Parser t () type
strip :: (Eq t, Show t) => t -> Parser t ()
strip c = Parser $ \ts ->
    let errmsg = "Required token " ++ show c ++ " was not found."
    in case ts of
        [] -> Left errmsg
        (x:xs) -> if c == x 
                  then pure ((), xs) 
                  else Left errmsg

strips :: (Eq t, Show t) => [t] -> Parser t ()
strips [] = pure ()
strips (x:xs) = strip x >> strips xs

-- used for parsing grouping characters of a parsed internal expression with arbitrary whitespace ignored
parseWithSurround :: Char -> Char -> Parser Char a -> Parser Char a
parseWithSurround t1 t2 p = parseWS *> (strip t1) *> parseWS *> p <* parseWS <* (strip t2)

-- oneOrMore :: Parser t a -> Parser t [a]
-- oneOrMore p = p `combine` p_rec
--     where p_rec = (p `combine` p_rec) <|> (pure [])

parseWS :: Parser Char ()
parseWS = Parser $ \ s -> pure ((), trimWS s)

trimWS :: String -> String
trimWS = dropWhile isSpace

grammarParse :: String -> Either String Grammar
grammarParse s = rulesParse s >>= \rules -> mkGrammar rules

rulesParse :: String -> Either String [ProductionRules]
rulesParse s = 
    let es = map (runParser parseLine) (lines s)
    in if null es 
        then Left "Error. No input lines provided."
        else g $ foldr f (Right []) $ zip [1..] es
        where
            g (Left msg) = Left $ "Syntax errors:\n" ++ msg
            g rs         = rs
            f (i,x) r = (case x of
                            Left msg ->
                                (case r of 
                                    Left msg_rest -> Left $ "Line " ++ show i ++ ": " ++ msg ++ "\n" ++ msg_rest
                                    Right _       -> Left $ "Line " ++ show i ++ ": " ++ msg ++ "\n")
                            Right (y, _) ->
                                (case r of
                                    Left _   -> r
                                    Right ys -> Right $ y : ys))
                        -- sequence $ map (\e -> e >>= \ (x, _) -> pure x) es

parseLine :: Parser Char (Variable, [Production])
parseLine = liftA2 (,) parseVariable (parseEq *> parseProds)
    where parseEq = parseWS *> strips "::="

parseVariable :: Parser Char Variable
parseVariable = parseWithSurround '<' '>' parseVarName

parseVarName :: Parser Char Variable
parseVarName = Parser $ \ts ->
    case ts of
        [] -> Left "Empty variable name not permitted."
        (x:xs) -> if isLetter x || x == '_' 
                  then g 
                  else Left $ "Variable name starts with invalid character (" ++ show x ++ ")."
            where g = let is_valid c = isAlphaNum c || c == '_' || c == '-'
                          (xs', xs'') = (takeWhile is_valid xs, dropWhile is_valid xs)
                      in Right (x:xs', xs'')

parseProds :: Parser Char [Production]
parseProds = someWithSeparator parseProd sep
    where sep = parseWS <* strip '|' <* parseWS

parseProd :: Parser Char Production
parseProd = some parseProdExpr

parseProdExpr :: Parser Char ProdExpr
parseProdExpr = parseTerminals <|> parseVar <|> parseGrp <|> parseMayb <|> parseSeq <|> failWithContext "Could not parse production expression. " nextTokenErr
    where parseGrp = fmap Grp $ parseWithSurround '(' ')' parseProds
          parseMayb = fmap Mayb $ parseWithSurround '[' ']' parseProds
          parseSeq = fmap Seq $ parseWithSurround '{' '}' parseProds
          parseVar = fmap Var parseVariable
          parseTerminals = fmap Terminals parseTerms

-- parseProdExpr_opt :: Parser Char ProdExpr
-- parseProdExpr_opt = (fmap Opts f) <|> parseProdExpr
--     where f = some $ parseProdExpr <* parseWS <* strip '|' <* parseWS

-- parseProdComp :: Parser Char ProdComp
-- parseProdComp = (fmap Var parseVar) <|> (fmap Terminals parseTerminals)

parseTerms :: Parser Char String
parseTerms = parseWithSurround '\'' '\'' f
    where f = Parser $ \ts -> 
              let is_valid c = isPrint c && c /= '\''
                  (ts', ts'') = (takeWhile is_valid ts, dropWhile is_valid ts)
              in Right (ts', ts'')

nextTokenErr :: String -> String
nextTokenErr [] = "Error encountered on empty string."
nextTokenErr ts = "Error parsing token word: " ++ (head $ words ts)


-- module ParseGrammar where

-- import Parser
-- import Grammar
-- import Data.Char (isSpace, isLetter, isAlphaNum, isPrint)
-- import Control.Applicative

-- -- used for stripping a token that doesn't add semantic value, but still important to syntax rules
-- -- partially apply with a token, and can be wrapped into an instance of Parser t () type
-- strip :: (Eq t, Show t) => t -> Parser t ()
-- strip c = Parser $ \ts ->
--     let errmsg = "Required token " ++ show c ++ " was not found."
--     in case ts of
--         [] -> Left errmsg
--         (x:xs) -> if c == x 
--                   then pure ((), xs) 
--                   else Left errmsg

-- strips :: (Eq t, Show t) => [t] -> Parser t ()
-- strips [] = pure ()
-- strips (x:xs) = strip x >> strips xs

-- -- used for parsing grouping characters of a parsed internal expression with arbitrary whitespace ignored
-- parseWithSurround :: Char -> Char -> Parser Char a -> Parser Char a
-- parseWithSurround t1 t2 p = parseWS *> (strip t1) *> parseWS *> p <* parseWS <* (strip t2)

-- -- oneOrMore :: Parser t a -> Parser t [a]
-- -- oneOrMore p = p `combine` p_rec
-- --     where p_rec = (p `combine` p_rec) <|> (pure [])

-- parseWS :: Parser Char ()
-- parseWS = Parser $ \ s -> pure ((), trimWS s)

-- trimWS :: String -> String
-- trimWS = dropWhile isSpace

-- grammarParse :: String -> Either String Grammar
-- grammarParse s = rulesParse s >>= \rules -> mkGrammar rules

-- rulesParse :: String -> Either String [ProductionRules]
-- rulesParse s = 
--     let es = map (runParser parseLine) (lines s)
--     in if null es 
--         then Left "Error. No input lines provided."
--         else g $ foldr f (Right []) $ zip [1..] $ filter (not . null) es
--         where
--             g (Left msg) = Left $ "Syntax errors:\n" ++ msg
--             g rs         = rs
--             f (i,x) r = (case x of
--                             Left msg ->
--                                 (case r of 
--                                     Left msg_rest -> Left $ "Line " ++ show i ++ ": " ++ msg ++ "\n" ++ msg_rest
--                                     Right _       -> Left $ "Line " ++ show i ++ ": " ++ msg ++ "\n")
--                             Right (y, _) ->
--                                 (case r of
--                                     Left _   -> r
--                                     Right ys -> Right $ y : ys))
--                         -- sequence $ map (\e -> e >>= \ (x, _) -> pure x) es

-- parseLine :: Parser Char (Variable, [Production])
-- parseLine = liftA2 (,) parseVariable (parseEq *> parseProds)
--     where parseEq = parseWS *> strips "::="

-- parseVariable :: Parser Char Variable
-- parseVariable = parseWithSurround '<' '>' parseVarName

-- parseVarName :: Parser Char Variable
-- parseVarName = Parser $ \ts ->
--     case ts of
--         [] -> Left "Empty variable name not permitted."
--         (x:xs) -> if isLetter x || x == '_' 
--                   then g 
--                   else Left $ "Variable name starts with invalid character (" ++ show x ++ ")."
--             where g = let is_valid c = isAlphaNum c || c == '_' || c == '-'
--                           (xs', xs'') = (takeWhile is_valid xs, dropWhile is_valid xs)
--                       in Right (x:xs', xs'')

-- parseProds :: Parser Char [Production]
-- parseProds = someWithSeparator parseProd sep
--     where sep = parseWS <* strip '|' <* parseWS

-- parseProd :: Parser Char Production
-- parseProd = some parseProdExpr

-- parseProdExpr :: Parser Char ProdExpr
-- parseProdExpr = parseTerminals <|> parseVar <|> parseGrp <|> parseMayb <|> parseSeq <|> failWithContext "Could not parse production expression. " nextTokenErr
--     where parseGrp = fmap Grp $ parseWithSurround '(' ')' parseProds
--           parseMayb = fmap Mayb $ parseWithSurround '[' ']' parseProds
--           parseSeq = fmap Seq $ parseWithSurround '{' '}' parseProds
--           parseVar = fmap Var parseVariable
--           parseTerminals = fmap Terminals parseTerms

-- -- parseProdExpr_opt :: Parser Char ProdExpr
-- -- parseProdExpr_opt = (fmap Opts f) <|> parseProdExpr
-- --     where f = some $ parseProdExpr <* parseWS <* strip '|' <* parseWS

-- -- parseProdComp :: Parser Char ProdComp
-- -- parseProdComp = (fmap Var parseVar) <|> (fmap Terminals parseTerminals)

-- parseTerms :: Parser Char String
-- parseTerms = parseWithSurround '\'' '\'' f
--     where f = Parser $ \ts -> 
--               let is_valid c = isPrint c -- && c /= '\''
--                   (ts', ts'') = (takeWhile is_valid ts, dropWhile is_valid ts)
--               in Right (ts', ts'')

-- nextTokenErr :: String -> String
-- nextTokenErr [] = "Error encountered on empty string."
-- nextTokenErr ts = "Error parsing token word: " ++ (head $ words ts)