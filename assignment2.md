# EECS 776 - Assignment 2
## Author: Jace Kline

```haskell
sum' :: [Int] -> Int
sum' []     = 0
sum' (x:xs) = x + sum xs

sum'' :: [Int] -> Int
sum'' xs = if null xs then 0 else head xs + sum (tail xs)

take' :: Int -> [a] -> [a]
take' 0 _      = []
take' _ []     = []
take' n (x:xs) = x : take (n-1) xs

take'' :: Int -> [a] -> [a]
take'' n xs = if (null xs || n == 0) then [] else head xs : take (n-1) (tail xs)

drop' :: Int -> [a] -> [a]
drop' 0 xs     = xs
drop' _ []     = []
drop' n (x:xs) = drop' (n-1) xs

drop'' :: Int -> [a] -> [a]
drop'' n xs = if null xs then []
              else
                if n == 0 then xs
                else drop'' (n-1) (tail xs)

last' :: [a] -> a
last' (x:[]) = x
last' (x:xs) = last' xs

last'' :: [a] -> a
last'' xs = case xs of
            (x:[]) -> x
            (x:ys) -> last'' ys
```
