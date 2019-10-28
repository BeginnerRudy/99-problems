module PartFive where

-- Logical equivalence predicates
not' :: Bool -> Bool
not' False = True
not' True = False

infixl 6 `and'`

and' :: Bool -> Bool -> Bool
and' True True = True
and' _ _ = False

infixl 6 `nand'`

nand' :: Bool -> Bool -> Bool
nand' a b = not' $ and' a b

infixl 4 `or'`

or' :: Bool -> Bool -> Bool
or' False False = False
or' _ _ = True

infixl 4 `nor'`

nor' :: Bool -> Bool -> Bool
nor' a b = not' $ or' a b

infixl 5 `xor'`

xor' :: Bool -> Bool -> Bool
xor' True False = True
xor' False True = True
xor' _ _ = False

impl' :: Bool -> Bool -> Bool
impl' a b = (not' a) `or'` b

infixl 3 `equ'`

equ' :: Bool -> Bool -> Bool
equ' True True = True
equ' False False = True
equ' _ _ = False

-- Helper predicates
powerset :: Int -> [a] -> [[a]]
powerset 0 _ = [[]]
powerset n domain = foldr func [] domain
  where
    func a xs = (map (\x -> a : x) $ powerset (n - 1) domain) ++ xs

-- Define predicates and/2, or/2, nand/2, nor/2, xor/2, impl/2 and equ/2 (for logical equivalence) which succeed or fail according to the result of their respective operations;



-- Truth tables for logical expressions (2). Continue problem P46 by defining and/2, or/2, etc as being operators.


-- Truth tables for logical expressions (3).



-- Gray codes.




-- Huffman codes.

