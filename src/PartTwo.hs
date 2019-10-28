module PartTwo where

import PartOne

data ListItem a
  = Single a
  | Multiple Int
             a
  deriving (Show, Eq)

encodeElement :: Eq a => (Int, a) -> ListItem a
encodeElement (a, b) =
  if a == 1
    then Single b
    else Multiple a b

encodeElement' :: Eq a => (Int, a) -> ListItem a
encodeElement' (1, x) = Single x
encodeElement' (n, x) = Multiple n x

-- Modified run-length encoding.



-- Decode a run-length encoded list.



-- Run-length encoding of a list (direct solution).



-- Duplicate the elements of a list.



-- Replicate the elements of a list a given number of times.



-- Drop every N'th element from a list.


-- Split a list into two parts.



-- Extract a slice from a list.



-- Rotate a list N places to the left.



-- Remove the K'th element from a list.
