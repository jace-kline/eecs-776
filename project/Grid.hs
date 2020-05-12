module Grid where

import Data.List

type Row = Int
type Col = Int
type Offset = Int
newtype Cell a = Cell { fromCell :: (Row, Col, a) }
    deriving (Show)
newtype Grid a = Grid { fromGrid :: [Cell a] }
    deriving (Show)

instance Eq (Cell a) where
    (Cell (r,c,_)) == (Cell (r2,c2,_)) = (r,c) == (r2,c2)

instance Ord (Cell a) where
    (Cell (r,c,_)) <= (Cell (r2,c2,_)) = (r < r2) || ((r == r2) && (c <= c2))

mkGrid :: Grid a
mkGrid = Grid []

width :: Grid a -> Int
width (Grid []) = 0
width (Grid xs) = 1 + (foldl (\a x -> if col x > a then col x else a) 0 xs)

row :: Cell a -> Int
row (Cell (r,_,_)) = r

col :: Cell a -> Int
col (Cell (_,c,_)) = c

changeLocation :: Cell a -> Offset -> Offset -> Cell a
changeLocation (Cell (r,c,x)) rOffset cOffset = Cell ((r + rOffset),(c + cOffset), x)

add :: Grid a -> Row -> Col -> a -> Grid a
add g r c x = addCell g $ Cell (r,c,x)

addCell :: Grid a -> Cell a -> Grid a
addCell (Grid cells) c = Grid $ nub $ insert c cells

-- adds list elements horizontally, starting from start index
addListH :: Grid a -> Row -> Col -> [a] -> Grid a
addListH g r c ls = foldl f g $ zip [0..] ls
    where f gr (i,x) = addCell gr (Cell (r, c + i, x))

addListV :: Grid a -> Row -> Col -> [a] -> Grid a
addListV g r c ls = foldl f g $ zip [0..] ls
    where f gr (i,x) = addCell gr (Cell (r + i, c, x))

-- merges sub-grid into another one by offsetting all cells
-- in the sub-grid by its relative top left (0,0) location in the other grid
mergeGrids :: Offset -> Offset -> Grid a -> Grid a -> Grid a
mergeGrids rOffset cOffset (Grid xs) g = 
    let xs_shifted = map (\c -> changeLocation c rOffset cOffset) xs
    in foldl addCell g xs_shifted

gridRow :: Row -> Grid a -> [Cell a]
gridRow r (Grid cells) = filter (rowEquals r) cells
    where rowEquals r' (Cell (r,_,_)) = r == r'

gridRows :: Grid a -> [[Cell a]]
gridRows (Grid cells) = groupBy (\x y -> row x == row y) cells



-- expands out the grid (by row) by placing Nothing in undefined cells
-- or Just <object> in defined cells
-- entirely undefined rows are denoted by Nothing
gridLists :: Grid a -> [Maybe [Maybe a]]
gridLists g = map (fmap (go 0)) $ f 0 $ gridRows g
    where
        f :: Row -> [[Cell a]] -> [Maybe [Cell a]]
        f _ [] = []
        f i (cs:css) = if null cs || row (head cs) > i
                       then Nothing : f (i + 1) (cs:css)
                       else Just cs : f (i + 1) css
        go :: Col -> [Cell a] -> [Maybe a]
        go _ [] = []
        go j cells@((Cell (_,c,x)):cs) = 
            if j == c
            then Just x : go (j + 1) cs
            else Nothing : go (j + 1) cells

showGrid :: Grid Char -> String
showGrid g = foldr (++) [] $ map f $ gridLists g
    where
        f m_xs = case m_xs of
            Nothing -> "\n\n"
            Just xs -> (map h xs) ++ "\n"
        h m_x = case m_x of
            Nothing -> ' '
            Just x  -> x