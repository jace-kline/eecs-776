module ParseGrammar where

import Parser
import Data.Char (isSpace)

-- used for stripping a token that doesn't add semantic value, but still important to syntax rules
-- partially apply with a token, and can be wrapped into an instance of Parser t () type
strip :: (Eq t, Show t) => t -> Parser t ()
strip c = Parser $ \ts ->
    let errmsg = "Required token " ++ show t ++ " was not found."
    in case ts of
        [] -> Left errmsg
        (x:xs) -> if c == x then pure ((), xs) else Left errmsg

strips :: (Eq t, Show t) => [t] -> Parser t ()
strips [] = pure ()
strips (x:xs) = strip x >> strips xs

-- used for parsing grouping characters of a parsed internal expression with arbitrary whitespace ignored
-- argument for fail msg to be provided for adding "external context" to the fail message
parseWithSurround :: Char -> Char -> Parser String a -> Parser String a
parseWithSurround t1 t2 p msg = liftA3 (\(_,y,_) -> y) (parseWS >> (strip t1)) >> parseWS) p (parseWS >> (strip t2)))


parseWS :: Parser String ()
parseWS = Parse $ /s -> pure ((), trimWS s)

trimWS :: String -> String
trimWS s = dropWhile isSpace s

parseLn :: Parser String (Variable, [Production])
parseLn = liftA2 (,) parseVar (parseEq >> parseProds)
    where parseEq :: Parser String ()
          parseEq = parseWS >> strips "::="

parseVar :: Parser String Variable
parseVar = parseWithSurround '<' '>' parseVarName

parseVarName :: Parser String Variable
parseVarName = Parser $ \ts ->
    case ts of
        [] -> Left "Empty variable name not permitted."
        (x:xs) -> if isLetter x || x == '_' then g else Left "Variable name starts with invalid character (" ++ show x ++ ")."
            where g = 
                let is_valid c = isAlphaNum c || c == '_' || c == '-'
                    (xs', xs'') = (takeWhile is_valid xs, dropWhile is_valid xs)
                in Right (x:xs', xs'')

oneOrMore :: Parser t a -> Parser t [a]
oneOrMore p = p `combine` p_rec
    where p_rec = (oneOrMore p) <|> (pure [])

parseProds :: Parser String [Production]
parseProds = oneOrMore parseProd

parseProd :: Parser String [ProdExpr]
parseProd = oneOrMore parseProdExpr

parseProdExpr :: Parser String ProdExpr
parseProdExpr = parseGrp <|> parseMayb <|> parseSeq <|> parsePrim
    where parseGrp = parseWithSurround '(' ')' parseProdExpr_opt
          parseMayb = fmap Mayb $ parseWithSurround '[' ']' parseProdExpr_opt
          parseSeq = fmap Seq $ parseWithSurround '{' '}' parseProdExpr_opt
          parsePrim = fmap Prim parseProdComp

parseProdExpr_opt :: Parser String ProdExpr
parseProdExpr_opt = (fmap Opts (parseProdExpr >> parseWS >> strip '|' >> parseProdExpr_opt)) <|> parseProdExpr

parseProdComp :: Parser String ProdComp
parseProdComp = (fmap Var parseVar) <|> (fmap Terminals parseTerminals)

parseTerminals :: Parser String String
parseTerminals = parseWithSurround ''' ''' f
    where f = Parse $ \ts -> 
              let is_valid c = isPrint c && c /= '''
                  (ts', ts'') = (takeWhile is_valid ts, dropWhile is_valid ts)
              in Right (ts', ts'')