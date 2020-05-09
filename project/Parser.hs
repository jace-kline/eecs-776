module Parser where

import Control.Monad
import Control.Applicative

data Parser t a = Parser ([t] -> Either String (a, [t]))

instance Functor (Parser t) where
    fmap f (Parser g) = Parser $ \ts -> g ts >>= (\(x, ts') -> (f x, ts'))

instance Applicative (Parser t) where
    pure x = Parser $ \ts -> pure (x, ts)
    liftA2 f (Parser g) (Parser h) = Parser $ \ts -> do
        (x, ts')  <- g ts
        (y, ts'') <- h ts'
        return (f x y, ts'')

instance Alternative (Parser t) where
    empty = Parser $ \_ -> Left "All parse options failed in alternative sequence."
    (Parser g) <|> (Parser h) = Parser $ \ts -> 
        case g ts of
            Left msg -> h ts
            x        -> x

instance Monad (Parser t) where
    return = pure
    (Parser g) >>= h = Parser $ \ts -> g ts >>= \ (x, ts') -> h x

combine :: Parse a -> Parse [a] -> Parse [a]
combine = liftA2 (:)