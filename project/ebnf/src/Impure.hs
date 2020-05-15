module Impure where

-- import Parser
import Grammar
import ParseGrammar
import Accepter
import Generator
import ParseTree
import Grid
import SampleGrammars
import System.Random
import Control.Exception
import System.Exit
import Control.Concurrent

main' :: IO ()
main' = do
    -- welcome message
    welcome
    try run :: IO (Either SomeException ())
    return ()

welcome :: IO ()
welcome = do
    putStrLn "Extended Backus-Naur Form (EBNF) Grammar Engine."
    putStrLn "Created by Jace Kline."

run :: IO ()
run = do
    -- load a grammar from file or terminal
    -- repeat until valid
    (s,g) <- getGrammar
    runWithGrammar s g

getGrammar :: IO (String, Grammar)
getGrammar = do
    opt <- getMenuOption loadGrammarMenu (1,5)
    case opt of
        1 -> tryParse =<< parseFromFile
        2 -> tryParse =<< parseFromInput
        3 -> tryParse =<< loadSampleGrammar
        4 -> showHelp >> getGrammar
        5 -> exitSuccess
        _ -> getGrammar
    where
        parseFromFile :: IO (String, Either String Grammar)
        parseFromFile = do
            filename <- promptString "Enter a file path:"
            e_str <- try $ readFile filename :: IO (Either SomeException String)
            case e_str of
                Left _    -> return ([], Left ("Failed to open or read input file '" ++ filename ++ "'."))
                Right str -> return (str, grammarParse str)
        parseFromInput :: IO (String, Either String Grammar)
        parseFromInput = do
            putStrLn "Enter an EBNF grammar on the lines below."
            putStrLn "Press the [ENTER] key twice (input empty line) to stop capturing input."
            ls <- readManyLines
            let s = unlines ls
            return (s, grammarParse s)
        tryParse :: (String, Either String Grammar) -> IO (String, Grammar)
        tryParse (s,e) = case e of
            Left m -> do
                putStr m
                getGrammar
            Right g -> return (s,g)
        loadSampleGrammar :: IO (String, Either String Grammar)
        loadSampleGrammar = do
            opt <- getMenuOption sampleGrammarMenu (1,3)
            case opt of
                1 -> return $ (gpaGrammarStr, grammarParse gpaGrammarStr)
                2 -> return $ (numberGrammarStr, grammarParse numberGrammarStr)
                _ -> getGrammar >>= \(s, g) -> return (s, Right g)
        showHelp :: IO ()
        showHelp = do
            putStrLn "Grammar for EBNF: "
            putStr ebnfGrammarStr
            putStr "\n\nSemantics for EBNF: \n\
            \<variable-name>            : If used on left-hand side, then you are defining production rules for this variable. If used on the right-hand side, then the variable can be expanded via any one of its production rules.\n\
            \'::='                      : Used to separate the left-hand side (variable) by the right-hand side (set of the variable's productions).\n\
            \'expression | expression'  : The disjunction (or) used in the right-hand side productions which indicates that the expression can be expanded by the right or left sub-expression, nondeterministically.\n\
            \'(expression)'             : Parenthesis are used for explicitly grouping a sub-expression.\n\
            \'[expression]'             : Square brackets indicate either 0 or 1 of the sub-expression is permitted.\n\
            \'{expression}'             : Curly brackets indicate that the expression can be expanded and concatenated successively 0 or more times.\n\
            \' 'terminal symbols 123' ' : Single quotes are used to encapsulate a sequence of terminal symbols.\n"
        


readManyLines :: IO [String]
readManyLines = do
    s <- getLine
    case s of
        [] -> return []
        xs -> do
            rest <- readManyLines
            return $ xs : rest



loadGrammarMenu :: IO ()
loadGrammarMenu = genMenu "EBNF Grammar Engine - Load Grammar:" $
                    [ "Load EBNF grammar from input file"
                    , "Input an EBNF grammar"
                    , "Load a sample grammar"
                    , "EBNF syntax help"
                    , "Exit program"]

sampleGrammarMenu :: IO ()
sampleGrammarMenu = genMenu "EBNF Grammar Engine - Sample Grammar Options:" $
                     [ "GPA"
                     , "Number"
                     , "None - go back to menu"]

runWithGrammar :: String -> Grammar -> IO ()
runWithGrammar s g = do
    opt <- getMenuOption mainMenu (1,5)
    case opt of
        1 -> putStr s >> runWithGrammar s g
        2 -> attemptGenerate >> runWithGrammar s g
        3 -> attemptCheck >> runWithGrammar s g
        4 -> run
        5 -> exitSuccess
        _ -> runWithGrammar s g
    where
        attemptCheck = promptString m0 >>= (\s -> putStrLn m1 >> boundedRun (checkStr s g)) >>= respond m4
        attemptGenerate = newStdGen >>= (\gen -> putStrLn m2 >> boundedRun (generateStr gen g)) >>= respond m5
        m0 = "Input an input string to determine whether it is produced by the grammar:"
        m1 = "Attempting to find a matching parse tree for the string. Allowing " ++ show bound_secs ++ " seconds to run..."
        m2 = "Attempting to generate a string from the grammar. Allowing " ++ show bound_secs ++ " seconds to run..."
        m3 = "Algorithm took more than " ++ show bound_secs ++ " seconds long and was terminated."
        m4 = "Match successful. Generated parse tree:\n\n"
        m5 = "String generation successful. Generated parse tree:\n\n"
        bound_secs :: Int
        bound_secs = 10
        boundedRun :: IO (Either String ParseTree) -> IO (Either String ParseTree)
        boundedRun action = do
            let usecs = bound_secs * 1000000 :: Int
            mvar <- newEmptyMVar
            -- The first action (thread) to finish will fill the mvar.
            -- True => The targeted action was completed within the time
            -- False => action not completed within the allotted time
            tids <- mapM (\act -> forkIO (act >>= putMVar mvar)) [threadDelay usecs >> return (Left m3), action]
            is_success <- takeMVar mvar
            mapM_ killThread tids
            return is_success
        respond :: String -> Either String ParseTree -> IO ()
        respond success_msg e = case e of
            Left m -> putStrLn m
            Right tree -> putStr success_msg >> printParseTree tree

promptString :: String -> IO String
promptString prompt = do
    putStrLn prompt
    getLine

checkStr :: String -> Grammar -> IO (Either String ParseTree)
checkStr s g = do
    case attemptMatch g s of
        Left msg -> return $ Left $ "Match failed. A parse tree could not be generated for this string. " ++ msg ++ "\n"
        Right tree -> return $ Right tree

generateStr :: StdGen -> Grammar -> IO (Either String ParseTree)
generateStr gen g = return $ Right $ generateTree g gen

printParseTree :: ParseTree -> IO ()
printParseTree tree = do
    putStr $ showGrid $ toShowGrid tree
    putStrLn $ "String: " ++ derivedString tree


getIntInput :: IO Int
getIntInput = do
    putStrLn "Enter an integer:"
    ret <- try $ readLn :: IO (Either SomeException Int)
    case ret of
        Left _ -> do
            putStrLn "Invalid input. Please input an integer."
            getIntInput
        Right x -> putStr "\n" >> return x


-- takes a menu display action and a range of inputs,
-- and displays + fetches until valid user input is received
getMenuOption :: IO () -> (Int,Int) -> IO Int
getMenuOption menu (l,h) = do
    menu
    opt <- getIntInput
    if opt >= l && opt <= h 
    then return opt
    else do
        putStrLn $ "Invalid input. Input integer must be between " ++ show l ++ " and " ++ show h ++ "."
        getMenuOption menu (l,h)

mainMenu :: IO ()
mainMenu = genMenu "EBNF Grammar Engine - Main Menu:" $ 
            [ "Print the loaded grammar"
            , "Generate a random string from the grammar"
            , "Check whether string is generated by the grammar"
            , "Load a new EBNF grammar"
            , "Exit program"]

genMenu :: String -> [String] -> IO ()
genMenu title options = (sequence_ (map putStrLn ("\n" : title : (map f $ zip [1..] options)))) >> putStr "\n\n"
    where f (i, opt) = show i ++ ". " ++ opt