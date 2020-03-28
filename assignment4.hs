{-# LANGUAGE GADTs, KindSignatures #-}

import Data.List (sort)

data IntTree :: * where
    IntEmpty :: IntTree
    IntNode  :: Int -> IntTree -> IntTree -> IntTree
    deriving Show

balanceIntTree :: [Int] -> IntTree
balanceIntTree [] = IntEmpty
balanceIntTree xs = IntNode (ord_xs !! center_index) (balanceIntTree xs1) (balanceIntTree xs2)
                        where 
                            ord_xs       = sort xs
                            center_index = length xs `div` 2
                            xs1 = take center_index ord_xs
                            xs2 = drop (center_index + 1) ord_xs

data Tree :: * -> * where
    Empty :: Tree a
    Node  :: a -> Tree a -> Tree a -> Tree a
    deriving Show

balance :: (Ord a) => [a] -> Tree a
balance [] = Empty
balance xs = Node (ord_xs !! center_index) (balance xs1) (balance xs2)
                where 
                    ord_xs       = sort xs
                    center_index = length xs `div` 2
                    xs1 = take center_index ord_xs
                    xs2 = drop (center_index + 1) ord_xs
