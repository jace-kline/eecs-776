
readALine :: IO String
readALine = do
    c <- getChar
    if c == '\n' then return []
                 else do
                     rest <- readALine
                     return $ rmBackspace (c:rest)

rmBackspace :: String -> String
rmBackspace []           = []
rmBackspace [c]          = if c == '\DEL' then [] else [c]
rmBackspace l@(c1:c2:cs) = if c2 == '\DEL' then cs else l