module PartThree where

import Data.Function
import Data.List
import Data.Ord (comparing)

-- Helper predicates
groupBy' :: (a -> a -> Bool) -> [a] -> [[a]]
groupBy' eq l = foldr group' [] l
  where
    group' e [] = [[e]]
    group' e (x:xs) =
      if eq e $ head x
        then ([e] ++ x) : xs
        else (group' e xs) ++ [x]

-- Insert an element at a given position into a list.



-- Create a list containing all integers within a given range.



-- Extract a given number of randomly selected elements from a list.


-- Lotto: Draw N different random numbers from the set 1..M.

-- Generate a random permutation of the elements of a list.


-- Generate the combinations of K distinct objects chosen from the N elements of a list.




-- Group the elements of a set into disjoint subsets.
-- a) Generate all the possibilities and returns them in a list.
-- b) Generalize the above predicate in a way that we can specify a list of group sizes and the predicate will return a list of groups.



-- Sorting a list of lists according to length of sublists
-- a) Sort the elements of this list according to their length.


-- b) Sort the elements of this list according to their length frequency.

