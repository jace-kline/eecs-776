module Impure where

-- import Parser
import Grammar
import ParseGrammar
import Accepter
import Generator
import ParseTree
-- import Grid
import System.Random
import Control.Exception
import System.Exit

main' :: IO ()
main' = do
    -- welcome message
    welcome
    run

welcome :: IO ()
welcome = do
    putStrLn "Extended Bachaus Naur Form (EBNF) Grammar Engine."
    putStrLn "Created by Jace Kline."

run :: IO ()
run = do
    -- load a grammar from file or terminal
    -- repeat until valid
    (s,g) <- getGrammar
    runWithGrammar s g

getGrammar :: IO (String, Grammar)
getGrammar = do
    opt <- getMenuOption loadGrammarMenu (1,4)
    case opt of
        1 -> tryParse parseFromFile
        2 -> tryParse parseFromInput
        3 -> tryParse loadSampleGrammar
        4 -> showHelp >> getGrammar
        5 -> exitSuccess
        _ -> getGrammar
    where
        parseFromFile :: IO (String, Either String Grammar)
        parseFromFile = do
            putStr "Enter a file name:\n> "
            fname <- getLine
            e_str <- try $ readFile fname
            return $
                case e_str of
                    Left _    -> (str, Left "Failed to open or read input file '" ++ filename ++ "'.")
                    Right str -> (str, grammarParse str)
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
            opt <- getMenuOption sampleGrammarMenu (1,5)
            case opt of
                1 -> return $ grammarParse gpaGrammarStr
                2 -> return $ grammarParse numberGrammarStr
                3 -> return $ grammarParse expressionGrammarStr
                4 -> return $ grammarParse ebnfGrammarStr
                _ -> getGrammar
        showHelp :: IO ()
        showHelp = do
            putStrLn "Grammar for EBNF: "
            putStr ebnfGrammarStr
            putStr "\nSemantics for EBNF: \n\
            \<variable-name>           : If used on left-hand side, then you are defining production rules for this variable. If used on the right-hand side, then the variable can be expanded via any one of its production rules.\n
            '::='                      : Used to separate the left-hand side (variable) by the right-hand side (set of the variable's productions).\n
            'expression | expression'  : The disjunction (or) used in the right-hand side productions which indicates that the expression can be expanded by the right or left sub-expression, nondeterministically.\n
            '(expression)'             : Parenthesis are used for explicitly grouping a sub-expression.\n
            '[expression]'             : Square brackets indicate either 0 or 1 of the sub-expression is permitted.\n
            '{expression}'             : Curly brackets indicate that the expression can be expanded and concatenated successively 0 or more times.\n
            ' 'terminal symbols 123' ' : Single quotes are used to encapsulate a sequence of terminal symbols.\n"
        


readManyLines :: IO [String]
readManyLines = do
    s <- readLine
    case s of
        [] -> return []
        xs -> do
            rest <- readManyLines
            return $ xs : rest



loadGrammarMenu :: IO ()
loadGrammarMenu = genMenu "EBNF Grammar Engine - Load Grammar:" $
                    [ "Load EBNF grammar from input file"
                    , "Input an EBNF grammar"
                    , "EBNF syntax help"
                    , "Exit program"]

sampleGrammarMenu :: IO ()
sampleGrammarMenu = genMenu "EBNF Grammar Engine - Sample Grammar Options:" $
                     [ "GPA"
                     , "Number"
                     , "Expression"
                     , "EBNF"
                     , "None - go back to menu"]

runWithGrammar :: String -> Grammar -> IO ()
runWithGrammar s g = do
    opt <- getMenuOption mainMenu (1,5)
    case opt of
        1 -> putStr s >> runWithGrammar s g
        2 -> generateStr g >> runWithGrammar s g
        3 -> checkStr g >> runWithGrammar s g
        4 -> run
        5 -> exitSuccess
        _ -> runWithGrammar s g

checkStr :: Grammar -> IO ()
checkStr g = do
    putStr "Input an input string to determine whether it is produced by the grammar.\n> "
    s <- readLine
    putStrLn "Attempting to find a matching parse tree for the string..."
    case attemptMatch g s of
        Left msg -> do
            putStr $ "Match failed. A parse tree could not be generated for this string." ++ msg ++ "\n"
        Right tree -> do
            putStrLn "Match successful. Generated parse tree:"
            printParseTree tree

generateStr :: Grammar -> IO ()
generateStr g = do
    putStrLn "Attempting to generate a string from the grammar..."
    gen <- getStdGen
    let tree = generateTree g gen
    putStrLn "String generation successful. Generated parse tree:"
    printParseTree tree

printParseTree :: ParseTree -> IO ()
printParseTree tree = do
    putStr $ show tree
    putStrLn $ "String: " ++ derivedString tree


getIntInput :: IO Int
getIntInput = do
    putStr "Enter an integer\n> "
    ret <- try $ readLn :: IO Int
    case ret of
        Left _ -> do
            putStrLn "Invalid input. Please input an integer."
            getIntInput
        Right x -> return x


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
genMenu title options = sequence_ (putStrLn title) : map (putStrLn . f) $ zip [1..] options
    where f (i, opt) = show i ++ ". " ++ opt