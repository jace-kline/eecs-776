module Main where

import Parser
import Grammar
import ParseGrammar
import Accepter
import Generator
import System.Random

main = do
    case grammarParse gpagrammarstr of
        Left m -> print m
        Right g -> gomain g 0 5
  
gomain :: Grammar -> Int -> Int -> IO ()
gomain g n m = do
    print $ generateTree g (mkStdGen n)
    if n < m then gomain g (n + 1) m else return ()

