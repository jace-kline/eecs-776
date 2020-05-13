module Main where

import Parser
import Grammar
import ParseGrammar
import Accepter
import Generator
import ParseTree
import Grid
import System.Random
import Control.Exception

main = do
    gen <- getStdGen
    let (i, _) = random gen :: (Int, StdGen)
    case grammarParse gpagrammarstr of
        Left m -> print m
        Right g -> gomain g i 0 5
  
gomain :: Grammar -> Int -> Int -> Int -> IO ()
gomain g i n m = do
    let t = generateTree g (mkStdGen (i + n))
    -- print t
    putStr $ showGrid $ toShowGrid t
    putStr $ "Derived String: " ++ derivedString t ++ "\n"
    -- print $ gridLists $ toShowGrid t
    -- putStr '\n' 
    if n < m then gomain g i (n + 1) m else return ()