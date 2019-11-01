module PartFour where

import PartOne

-- Determine whether a given integer number is prime.
p31 :: Int -> Bool
p31 n = length (filter (\x -> n `mod` x == 0)  [2..(n-1)]) == 0


-- Determine the greatest common divisor of two positive integer numbers. Use Euclid's algorithm.
p32 :: Int -> Int -> Int
p32 a b = maximum (findCommon a_factors b_factors)
    where 
        a_factors = [x | x <- [1..a], a `mod` x == 0]
        b_factors = [x | x <- [1..b], b `mod` x == 0]

findCommon :: Eq a => [a] -> [a] -> [a]
findCommon _ [] = []
findCommon [] _ = []
findCommon (x:xs) ys 
    | x `in'` ys = x : findCommon xs ys
    | otherwise = findCommon xs ys 

in' :: Eq a => a -> [a] -> Bool
in' _ [] = False
in' n (x:xs)
    | n == x = True
    | otherwise = in' n xs

-- Determine whether two positive integer numbers are coprime. Two numbers are coprime if their greatest common divisor equals 1.
p33 :: Int -> Int -> Bool
p33 a b =  p32 a b == 1


-- Calculate Euler's totient function phi(m).
p34 :: Int -> Int
p34 x = length $ filter (p33 x) [1..(x-1)] 

-- Determine the prime factors of a given positive integer. Construct a flat list containing the prime factors in ascending order.
-- cannot solve
-- p35 :: Int -> [Int]


-- Determine the prime factors of a given positive integer.
-- did similar problem before 


-- Calculate Euler's totient function phi(m) (improved).



-- A list of prime numbers.
p39 :: Int -> Int -> [Int]
p39 a b = filter p31 [a..b]


-- Goldbach's conjecture.
p40 :: Int -> (Int, Int)
p40 x
    | x == 1  = error "Please enter number larger than 1."
    | x `mod` 2 /= 0 = error "Please enter an even number."
    | otherwise = (elem, x - elem)
    where 
        primes = filter p31 [2..(x-1)]
        list = filter (\y -> p31 (x - y)) primes
        elem = head list

-- Given a range of integers by its lower and upper limit, print a list of all even numbers and their Goldbach composition.
p41a :: Int -> Int -> [(Int, Int)]
p41a a b = map p40 (filter even [a..b])

-- Very rarely, the primes are both bigger than say 50. Try to find out how many such cases there are in the range 2..3000.
p41b :: Int -> Int -> Int -> [(Int, Int)]
p41b a b limit = filter (\(x, y) -> x >= limit && y >= limit) (p41a a b)