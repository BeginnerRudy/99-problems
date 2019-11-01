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
-- index start from 1
p21 :: a -> [a] -> Int -> [a]
p21 elem list index
    | index > len || index < 1 = error "Index error."
    | otherwise = take (index - 1) list ++ [elem] ++ drop (index - 1) list
    where len = length list


-- Create a list containing all integers within a given range.
p22 :: Int -> Int -> [Int]
p22 a b 
    | a > b = error "Range error"
    | a == b = [a]
    | otherwise = a : p22 (a+1) b


-- Extract a given number of randomly selected elements from a list.
-- do not know how to do random


-- Lotto: Draw N different random numbers from the set 1..M.

-- Generate a random permutation of the elements of a list.


-- Generate the combinations of K distinct objects chosen from the N elements of a list.
p26 :: Eq a => Int -> [a] -> [[a]]
p26 k list
    | k >= length list = [list]
    | otherwise = removeDuplicates 0 $ combi k list (sublist list)

removeDuplicates :: Eq a => Int -> [a]  -> [a]
removeDuplicates _ [] = []
removeDuplicates n list@(x:xs) 
    | n > length list = list
    | otherwise = removeDuplicates (n+1) $ removeDuplicate (list!!n) list

removeDuplicate :: Eq a => a -> [a] -> [a]
removeDuplicate _ [] = []
removeDuplicate p (x:xs) 
    | p == x = removeDuplicate p xs
    | otherwise = x : removeDuplicate p xs

combi :: Eq a => Int -> [a] -> [[a]] -> [[a]]
combi 1 _ acc = acc 
combi k list acc = combi (k-1) list (addOneLayer acc list)


addOneLayer :: Eq a => [[a]] -> [a] -> [[a]]
addOneLayer [] _ = []
addOneLayer (x:xs) list = result ++ addOneLayer xs list
    where 
        pool = sublist $ removeFromList x list
        n = length pool
        result = zipWith (++) (replicate n x) pool
        

sublist :: Eq a => [a] -> [[a]]
sublist [] = []
sublist (x:xs) = [x] : sublist xs

removeFromList :: Eq a => [a] -> [a] -> [a]
removeFromList [] list = list
removeFromList (x:xs) list = removeFromList xs (remove x list)

remove :: Eq a => a -> [a] -> [a]
remove _ [] = []
remove n list = take (index - 1) list ++ drop index list
    where index = findElem n list

findElem :: Eq a => a -> [a] -> Int
findElem _ [] = error "Not Found"
findElem n (x:xs)
    | n == x = 1
    | otherwise = 1 + findElem n xs

-- Group the elements of a set into disjoint subsets.
-- a) Generate all the possibilities and returns them in a list.

-- b) Generalize the above predicate in a way that we can specify a list of group sizes and the predicate will return a list of groups.



-- Sorting a list of lists according to length of sublists
-- a) Sort the elements of this list according to their length.
q28a :: [[a]] -> [[a]]
q28a list = transformBack $ sortIt $ transform list

transform :: [[a]] -> [(Int, [a])]
transform [] = []
transform (x:xs) = (length x, x) : transform xs

transformBack :: [(Int, [a])] -> [[a]]
transformBack [] = []
transformBack ((_, x):xs) = x : transformBack xs 

extractEle :: (Int, [a]) -> Int
extractEle (n, _) = n

sortIt :: [(Int, [a])] -> [(Int, [a])]
sortIt [] = []
sortIt [x] = [x]
sortIt list = sortIt left ++ [pivot] ++ sortIt right
    where
        index = (length list) `div` 2 
        pivot = list !! index
        remainList = (take (index) list) ++ drop (index+1) list
        (left, right) = partition' pivot remainList
        
partition' :: (Int, [a]) -> [(Int, [a])] -> ([(Int, [a])], [(Int, [a])])
partition' _ [] = ([], [])
partition' (pivot, _) [elem@(x, _)]
    | x <= pivot = ([elem], [])
    | otherwise = ([], [elem])
partition' p@(pivot, _) (elem@(x, _):xs)
    | x <= pivot = (elem:ys, zs)
    | otherwise = (ys, elem:zs)
    where (ys, zs) = partition' p xs

-- b) Sort the elements of this list according to their length frequency.

