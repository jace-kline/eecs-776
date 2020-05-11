module Exam where

import Data.Time.Calendar
import Control.Applicative

dropEverySecond :: [Int] -> [Int]
dropEverySecond xs = go 1 xs
    where
        go _ []     = []
        go n (x:xs) = if 2 `divides` n 
                      then go (n + 1) xs
                      else x : go (n + 1) xs
        divides m n = n `mod` m == 0

-- main = do
--     day <- getCurrentTime
--     -- let day = utctDay time
--     print time

data Fail a = Fail deriving Show

instance Functor Fail where
    fmap f x = Fail

instance Applicative Fail where
    pure a = Fail
    x <*> y = Fail

instance Monad Fail where
    return a = Fail
    x >>= y = Fail

sequence' :: (Applicative f) => [f a] -> f [a]
sequence' = foldr (liftA2 (:)) (pure [])