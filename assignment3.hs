{-# LANGUAGE GADTs, KindSignatures #-}

import Prelude hiding (lookup) 

-- Author: Jace Kline 2881618
-- Implement 2 versions of a Map data type. The functions empty, lookup, insert, delete should be implemtented.

newtype Map k a = Map_ [(k, a)] deriving (Show)

empty :: Map k a
empty = Map_ []

lookup :: (Eq k) => k -> Map k a -> Maybe a
lookup _ (Map_ [])     = Nothing
lookup k (Map_ ((x,y):xs)) = if k == x 
                             then Just y 
                             else lookup k (Map_ xs)

insert :: k -> a -> Map k a -> Map k a
insert k x (Map_ xs) = Map_ $ (k,x) : xs

delete :: (Eq k) => k -> Map k a -> Map k a
delete k (Map_ xs) = Map_ $ case xs of
                            []          -> []
                            ((x,y):xys) -> 
                                let (Map_ r) = delete k (Map_ xys)
                                in if k == x
                                   then r
                                   else (x,y):r


data Map' :: * -> * -> * where
    Empty :: Map' k a
    Pair  :: k -> a -> Map' k a -> Map' k a
    deriving (Show)

empty' :: Map' k a
empty' = Empty

lookup' :: (Eq k) => k -> Map' k a -> Maybe a
lookup' _ Empty        = Nothing
lookup' k (Pair x y r) = if k == x
                         then Just y
                         else lookup' k r

insert' :: k -> a -> Map' k a -> Map' k a
insert' = Pair

delete' :: (Eq k) => k -> Map' k a -> Map' k a
delete' _ Empty        = Empty
delete' k (Pair x y r) = 
    let rest = delete' k r
    in if k == x
       then rest
       else Pair x y rest

