module PartOne where

data NestedList a
  = Elem a
  | List [NestedList a]

-- Find the last element of a list.
p1 :: [a] -> a
p1 [] = error "Empty List, index out of range."
p1 [x] = x
p1 (x:xs) = p1 xs


-- Find the last but one element of a list.
p2 :: [a] -> a
p2 list | length list < 2 = error "Index out of range."
p2 (x:(y:[])) = x
p2 (x:xs) = p2 xs 


-- Find the K'th element of a list. The first element in the list is number 1.
p3 :: Int -> [a] -> a
p3 k list | length list < k || k < 1 = error "Index out of range"
p3 k (x:xs) 
  | k == 1 = x
  | otherwise = p3 (k-1) xs


-- Find the number of elements of a list.
p4 :: [a] -> Int
p4 [] = 0
p4 (x:xs) = 1 + (p4 xs)


-- Reverse a list.
p5 :: [a] -> [a]
-- p5 [] = []
-- p5 (x:xs) = p5 xs ++ [x] 

-- the solution below is not created by me
p5 = foldl (flip (:)) []


-- Find out whether a list is a palindrome. A palindrome can be read forward or backward; e.g. (x a m a x).
p6 :: Eq a => [a] -> Bool
p6 [] = True
p6 list 
  | len `mod` 2 == 0 = identical_list (take half_len list) (reverse (drop half_len list))
  | otherwise = identical_list (take half_len list) (reverse (drop (half_len+1) list)) 
  where
      len = length list
      half_len = len `div` 2

-- assume two lists are of same length
identical_list :: Eq a => [a] -> [a] -> Bool
identical_list [] [] = True
identical_list (x:xs) (y:ys)
    | x /= y = False
    | otherwise = identical_list xs ys

-- Flatten a nested list structure.
q7 :: NestedList a -> [a]
q7 (Elem a) = [a]
q7 (List []) = []
q7 (List (x:xs)) = q7 x ++ q7 (List xs)

-- Eliminate consecutive duplicates of list elements.
p8 :: Eq a => [a] -> [a]
p8 [] = []
p8 [x] = [x]
p8 (x1:(x2:xs)) 
    | x1 == x2 = p8 (x2:xs)
    | otherwise = x1 : p8 (x2:xs)


-- Pack consecutive duplicates of list elements into sublists.
p9 :: Eq a => [a] -> [[a]]
p9 [] = []
p9 list = sub : p9 remain
    where
        index = findDiffIndex list 1
        sub = take index list
        remain = drop index list

-- assume the list is not empty
findDiffIndex :: Eq a => [a] -> Int -> Int
findDiffIndex  [x] acc = acc
findDiffIndex (x1:x2:xs)  acc
    | x1 == x2 = findDiffIndex (x2:xs) (acc+1)
    | otherwise = acc


-- Run-length encoding of a list.

p10 :: Eq a => [a] -> [(Int, a)]
p10 list = count (p9 list)

count :: Eq a => [[a]] -> [(Int, a)]
count [] = []
count (x:xs) = c : count xs
      where c =  ((length x), (head x))