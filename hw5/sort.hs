
import Data.List (nub)
import Data.Char (isUpper, isLower, toLower)
import System.IO
import System.Environment

{-
The sort hierarchy of the Linux 'sort' command is as follows:
    - lines that start with numbers are 'less than' lines that start with characters
        (already Haskell String semantics)
    - if same first character, then lowercase version string < uppercase version string
        (Created a SortStr data type to achieve this functionality)
    - otherwise, standard alphabetical order
-}

newtype SortStr = SortStr { toString :: String } deriving (Eq, Read)

instance Show SortStr where
    show (SortStr s) = show s

instance Ord SortStr where
    (SortStr s1) <= (SortStr s2) = case s1 of
        []     -> True
        (x:xs) -> case s2 of
            []     -> False
            (y:ys) -> if (toLower x == toLower y)
                      then case (isLower x, isLower y) of
                        (True, False) -> True
                        (False, True) -> False
                        _             -> def
                      else def
                        where def = (map toLower (x:xs)) <= (map toLower (y:ys))

{-
    The Option data type represents the options that can be applied when running the program
        -r option --> Reverse (reverses the sort order)
        -n option --> Numeric (attempts to parse the first column of file as numbers, then sorts)
        -u option --> Unique  (removes duplicate lines)
        -c option --> Compare (outputs a report that tells which line the original file became unordered at)
-}

data Option = Reverse | Numeric | Unique | Compare
    deriving (Eq, Show)

main :: IO ()
main = do
    args <- getArgs
    let (options, files) = parseInput args
    case (options, files) of
        (Left msg, _)       -> putStrLn msg
        (Right opts, files) -> sortCommand opts files

sortCommand :: [Option] -> [FilePath] -> IO ()
sortCommand opts files = do
    input <- case files of 
                [] -> getContents
                fs -> readFiles fs
    let ls = nonemptys $ lines input
    let sorted = (if Numeric `elem` opts then sortNumeric else sortAlphabetic) ls
    if Compare `elem` opts 
    then putStrLn $ compareOutput ls sorted
    else sequence_ $ map putStrLn $ optsApplied sorted
    where
        nonemptys = filter (\xs -> not (null xs))
        compareOutput ls sorted = let comp = compareLines ls sorted
                                   in case comp of
                                        Nothing -> "sort: file already sorted"
                                        Just n  -> "sort: disorder at line " ++ show n ++ ": " ++ (ls !! (n - 1))
        optsApplied sorted = ((if Unique `elem` opts then nub else id) . (if Reverse `elem` opts then reverse else id)) sorted

readFiles :: [FilePath] -> IO String
readFiles [] = return ""
readFiles (f:fs) = do
    this <- readFile f
    rest <- readFiles fs
    return $ this ++ "\n" ++ rest
        
compareLines :: [String] -> [String] -> Maybe Int
compareLines xs ys = go 1 xs ys
    where go :: Int -> [String] -> [String] -> Maybe Int
          go i [] _          = Nothing
          go i _ []          = Nothing
          go i (x:xs) (y:ys) = if x == y 
                               then go (i+1) xs ys >>= \p -> return p
                               else return i

sortNumeric :: [String] -> [String]
sortNumeric xs = let keyified = [(((read :: String -> Double) . head . words) x, x) | x <- xs] :: [(Double, String)]
                 in map snd $ sort keyified

sortAlphabetic :: [String] -> [String]
sortAlphabetic xs = map toString $ sort $ map SortStr xs

parseInput :: [[Char]] -> (Either String [Option], [FilePath])
parseInput [] = (Right [], [])
parseInput (x:xs) = case x of
    [] -> parseInput xs
    (c:cs) -> case c of
        '-' -> let opts              = parseOpts cs
                   (rest_opts, file) = parseInput xs
               in ((do 
                   os <- opts
                   os2 <- rest_opts
                   return (nub (os ++ os2)))
                   , file)
        _   -> (Right [], x:xs)

parseOpts :: [Char] -> Either String [Option]
parseOpts []     = Left "Empty option list provided following '-' token."
parseOpts (c:cs) = sequence $ go (c:cs)
    where go :: [Char] -> [Either String Option]
          go []          = []
          go (c:cs) =
              let opt = case c of
                            'r' -> return Reverse
                            'n' -> return Numeric
                            'u' -> return Unique
                            'c' -> return Compare
                            _   -> Left $ "Unknown option flag '" ++ c : "' provided in option list."
              in opt : go cs
        
       
sort :: (Ord a) => [a] -> [a]
sort []       = []
sort [x]      = [x]
sort (x:y:xs) = let (t:ts) = if x <= y 
                             then x : sort (y:xs)
                             else y : sort (x:xs)
                in propogate t ts
                where
                    propogate :: (Ord b) => b -> [b] -> [b]
                    propogate t []     = [t]
                    propogate t (v:vs) = if t <= v then t : v : vs else v : (propogate t vs) 

