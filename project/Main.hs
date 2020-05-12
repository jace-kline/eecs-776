module Main where

import Parser
import Grammar
import ParseGrammar
import Accepter
import Generator
import ParseTree
import Grid
import System.Random

main = do
    case grammarParse numbergrammarstr of
        Left m -> print m
        Right g -> gomain g 0 5
  
gomain :: Grammar -> Int -> Int -> IO ()
gomain g n m = do
    let t = generateTree g (mkStdGen n)
    -- print t
    putStr $ showGrid $ toShowGrid t
    -- print $ gridLists $ toShowGrid t
    -- putStr '\n' 
    if n < m then gomain g (n + 1) m else return ()