module PartTwo where

import PartOne

-- Modified run-length encoding.
data Counter a = Multiple Int a | Single a
    deriving Show

p11 :: Eq a => [a] -> [Counter a]
p11 list = counts (p9 list)

counts :: Eq a => [[a]] -> [Counter a]
counts [] = []
counts (x:xs) 
    | n > 1 = (Multiple n elem) : counts xs
    | otherwise  = (Single elem) : counts xs
    where
        n = length x
        elem = head x


-- Decode a run-length encoded list.
p12 :: [Counter a] -> [a]
p12 [] = []
p12 (x:xs) = decode x ++ p12 xs

decode :: (Counter a)-> [a]
decode (Single a) = [a]
decode (Multiple n a) = take n (repeat a)


-- Run-length encoding of a list (direct solution).



-- Duplicate the elements of a list.
p14 :: [a] -> [a]
p14 [] = []
p14 (x:xs) = x:x:p14 xs


-- Replicate the elements of a list a given number of times.
p15 :: [a] -> Int -> [a]
p15 [] _ = []
p15 (x:xs) n
    | n < 1 = error "Please enter number larger than 0"
    | otherwise = take n (repeat x) ++ (p15 xs n)


-- Drop every N'th element from a list.
-- assmue is larger than 1 and less than the length of the list
p16 :: Int -> [a] -> [a]
p16 _ [] =[]
p16 n list = front ++ p16 n remain
    where
        front = take (n-1) list
        remain = drop n list


-- Split a list into two parts. No pre-defined predicates allowed.
-- not solved by me!!!!!!!!!!
p17 :: [a] -> Int -> ([a], [a])
p17 [] _ = ([], [])
p17 list@(x:xs) n 
    | n == 0 = ([], list)
    | otherwise = (x:zs, ys)
    where (zs, ys) = p17 xs (n-1)


-- Extract a slice from a list.
p18 :: [a] -> Int -> Int -> [a]
p18 list start end 
    | end > n = error "Index error: index out of range"
    | otherwise = drop (start - 1) (take end list)
    where n = length list


-- Rotate a list N places to the left.
p19 :: [a] -> Int -> [a]
p19 list n 
    | n == 1 = list
    | n > 1 = drop n list ++ take n list 
    | n < 0 = drop (len + n) list ++ take (len + n) list
    | otherwise = error "Index error"
    where len = length list


-- Remove the K'th element from a list.
p20 :: Int -> [a] -> (a, [a])
p20 n list = (elem, new_list)
    where 
        elem = list!!(n-1)
        new_list = take (n-1) list ++ drop n list