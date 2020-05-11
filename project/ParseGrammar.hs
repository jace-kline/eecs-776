module ParseGrammar where

import Parser
import Grammar
import Data.Char (isSpace, isLetter, isAlphaNum, isPrint)
import Control.Applicative (liftA2, (<|>))

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
-- argument for fail msg to be provided for adding "external context" to the fail message
parseWithSurround :: Char -> Char -> Parser Char a -> Parser Char a
parseWithSurround t1 t2 p = parseWS *> (strip t1) *> parseWS *> p <* parseWS <* (strip t2)

oneOrMore :: Parser t a -> Parser t [a]
oneOrMore p = p `combine` p_rec
    where p_rec = (p `combine` p_rec) <|> (pure [])

parseWS :: Parser Char ()
parseWS = Parser $ \ s -> pure ((), trimWS s)

trimWS :: String -> String
trimWS = dropWhile isSpace

grammarParse :: String -> Either String [(Variable, [Production])]
grammarParse s = let es = map (runParser parseLine) (lines s)
                 in if null es 
                    then Left "Error. No input lines provided."
                    else sequence $ map (\e -> e >>= \ (x, _) -> pure x) es

parseLine :: Parser Char (Variable, [Production])
parseLine = liftA2 (,) parseVar (parseEq *> parseProds)
    where parseEq = parseWS *> strips "::="

parseVar :: Parser Char Variable
parseVar = parseWithSurround '<' '>' parseVarName

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
parseProds = f <|> (fmap (\x -> [x]) parseProd)
    where f = oneOrMore $ parseProd <* parseWS <* strip '|'

parseProd :: Parser Char Production
parseProd = oneOrMore parseProdExpr

parseProdExpr :: Parser Char ProdExpr
parseProdExpr = parseGrp <|> parseMayb <|> parseSeq <|> parsePrim
    where parseGrp = parseWithSurround '(' ')' parseProdExpr_opt
          parseMayb = fmap Mayb $ parseWithSurround '[' ']' parseProdExpr_opt
          parseSeq = fmap Seq $ parseWithSurround '{' '}' parseProdExpr_opt
          parsePrim = fmap Prim parseProdComp

parseProdExpr_opt :: Parser Char ProdExpr
parseProdExpr_opt = (fmap Opts f) <|> parseProdExpr
    where f = oneOrMore $ parseProdExpr <* parseWS <* strip '|'

parseProdComp :: Parser Char ProdComp
parseProdComp = (fmap Var parseVar) <|> (fmap Terminals parseTerminals)

parseTerminals :: Parser Char String
parseTerminals = parseWithSurround '\'' '\'' f
    where f = Parser $ \ts -> 
              let is_valid c = isPrint c && c /= '\''
                  (ts', ts'') = (takeWhile is_valid ts, dropWhile is_valid ts)
              in Right (ts', ts'')