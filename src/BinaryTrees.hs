data Tree k = Empty | Branch k  (Tree k) (Tree k)
    deriving (Eq, Ord, Show)

-- (*) Check whether a given term represents a binary tree
--　Answer: Non-solution:
-- Haskell's type system ensures that all terms of type Tree a are binary 
-- trees: it is just not possible to construct an invalid tree with this type. 
-- Hence, it is redundant to introduce a predicate to check this property:
-- it would always return True.

-- (**) Construct completely balanced binary trees (list comprehension)
q55 :: Int -> [Tree Char]
q55 0 = [Empty]
q55 n = [Branch 'x' left right| i <- [q..(q+r)], left <- q55 i, right <- q55 (n-1-i)]
    where (q, r) = (n - 1) `quotRem` 2

-- (**) Symmetric binary trees
q56 :: (Eq v, Ord v, Show v) => Tree v -> Bool
q56 Empty = True
q56 (Branch _ l r) = identical l r

identical :: (Eq v, Ord v, Show v) => Tree v -> Tree v -> Bool
identical Empty Empty = True
identical Empty (Branch _ Empty Empty) = True
identical Empty _ = False
identical (Branch _ Empty Empty) Empty = True
identical _ Empty = False
identical (Branch _ la ra) (Branch _ lb rb) = identical la ra && identical lb rb

-- (**) Binary search trees (dictionaries)
q57 :: (Eq v, Ord v, Show v) => [v] -> Tree v
q57 = foldl insert Empty 

insert :: (Eq v, Ord v, Show v) => Tree v -> v -> Tree v
insert Empty x = Branch x Empty Empty
insert (Branch p l r) x 
    | x < p = Branch p (insert l x) r
    | otherwise = Branch p l (insert r x)

-- (**) Generate-and-test paradigm

q58 :: Int -> [Tree Char]
q58 = filter q56 . q55

-- (**) Construct height-balanced binary trees
q59 :: Int -> [Tree Char]
q59 0 = [Empty]
q59 1 = [Branch 'x' Empty Empty]
q59 n = [Branch 'x' left right | i <- [(h-1)..h], left <- q59 i, right <- q59 (h - (((h - i)+1) `mod` 2))]
    where h = n - 1

-- (**) Construct height-balanced binary trees with a given number of nodes
q60 :: 