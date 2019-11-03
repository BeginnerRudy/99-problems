data Tree v = Empty | Branch v (Tree v) (Tree v)
    deriving (Eq, Ord, Show)

tree4 = Branch 1 (Branch 2 Empty (Branch 4 Empty Empty))
                 (Branch 2 Empty Empty)

-- Count the leaves of a binary tree
q61 :: (Ord v, Eq v, Show v) =>  Tree v -> Int
q61 Empty = 0
q61 (Branch _ Empty Empty) = 1
q61 (Branch _ l r) =  (q61 l) + (q61 r) 

-- Collect the leaves of a binary tree in a list
q61a :: (Ord v, Eq v, Show v) =>  Tree v -> [v]
q61a Empty = []
q61a (Branch x Empty Empty) = [x]
q61a (Branch _ l r) = q61a l ++ q61a r

-- Collect the internal nodes of a binary tree in a list
q62 :: (Ord v, Eq v, Show v) =>  Tree v -> [v] 
q62 Empty = []
q62 (Branch _ Empty Empty) = []
q62 (Branch x l r) = [x] ++ q62 l ++ q62 r

-- Collect the nodes at a given level in a list
-- the root node is at level 1
q62b :: (Ord v, Eq v, Show v) =>  Tree v -> Int -> [v] 
q62b Empty _ = []
q62b (Branch x l r) n 
    | n == 1 = [x]
    | otherwise = q62b l (n-1) ++ q62b r (n-1)

-- Construct a complete binary tree
q63 :: Int -> Tree Char
q63 0 = Empty
q63 1 = Branch 'x' Empty Empty
q63 n = Branch 'x' left right
    where
        n_right = (n-1) `div` 2
        r = (n-1) `mod` 2
        n_left = n_right + r
        left = q63 n_left
        right = q63 n_right 

-- Ex 64, 65, 66 are about how to layout a binary tree, which is used for visuallizing a binary search tree.
tree64 = Branch 'n'
                (Branch 'k'
                        (Branch 'c'
                                (Branch 'a' Empty Empty)
                                (Branch 'h'
                                        (Branch 'g'
                                                (Branch 'e' Empty Empty)
                                                Empty
                                        )
                                        Empty
                                )
                        )
                        (Branch 'm' Empty Empty)
                )
                (Branch 'u'
                        (Branch 'p'
                                Empty
                                (Branch 's'
                                        (Branch 'q' Empty Empty)
                                        Empty
                                )
                        )
                        Empty
                )


-- Binary Tree Layout Algorithm. 
-- Write a function to annotate each node of the tree with a position, 
-- where (1,1) in the top left corner or the rectangle bounding the drawn tree.

-- Not solved by me
type Pos = (Int, Int)
q64 :: Tree Char -> Tree (Char, Pos)
q64 t = fst (layout 1 1 t)

layout :: Int -> Int -> Tree Char -> (Tree (Char, Pos), Int)
layout x y Empty = (Empty, x)
layout x y (Branch n l r) = (Branch (n, (x', y)) l' r', x'')
    where
        (l', x') = layout x (y+1) l
        (r', x'') = layout (x'+1) (y+1) r




-- An alternative layout method 


-- A string representation of binary trees, string to BST.
-- not solved by myself


-- Preorder and inorder sequences of binary trees. 
-- We consider binary trees with nodes that are identified by single 
-- lower-case letters, as in the example of problem P67.

-- a) Write predicates preorder/2 and inorder/2 that construct the preorder 
-- and inorder sequence of a given binary tree, respectively. The results 
-- should be atoms, e.g. 'abdecfg' for the preorder sequence of the example 
-- in problem P67.

preorder :: Tree Char -> String
preorder Empty = ""
preorder (Branch x l r)  = [x] ++ preorder l ++ preorder r

inorder :: Tree Char -> String
inorder Empty = ""
inorder (Branch x l r) = inorder l ++ [x] ++ inorder r

-- b) Can you use preorder/2 from problem part a) in the reverse direction; 
-- i.e. given a preorder sequence, construct a corresponding tree? 
-- If not, make the necessary arrangements.

-- No, since I cannot determine the actual depth of the tree.

-- c) If both the preorder sequence and the inorder sequence of the nodes of
-- a binary tree are given, then the tree is determined unambiguously. 
-- Write a predicate pre_in_tree/3 that does the job.


-- Dotstring representation of binary trees.
-- easy, not do