-- ----------------------------------------
--
-- simple operations on lists

module Data.List.SimpleOps
where

import Prelude hiding (splitAt)

-- ----------------------------------------

-- | The nub function removes duplicate elements from a list.
--
-- In particular, it keeps only the first occurrence of each element.
-- (The name nub means `essence'.)
--
-- Complexity class?

-- .1 nub with filter

nub :: Eq a => [a] -> [a]
nub [] = []
nub (x : xs) = x: nub( filter ( /= x) xs)    



--(sum . map square . filter even) [1..10] = 220



-- .2 nub with list comprehension

nub' :: Eq a => [a] -> [a]
nub' [] = []
nub' (x : xs) = x:nub([ y   | y <- xs, y /= x])



-- .3 nub with foldr
-- after chapter about folds

nub'' :: Eq a => [a] -> [a]
nub'' =  foldr (\x xs -> x: filter (/=x) xs) []

-- ----------------------------------------

-- | 'splitAt' @n xs@ returns a tuple where first element is @xs@ prefix of
-- length @n@ and second element is the remainder of the list:
--
-- > splitAt 6 "Hello World!" == ("Hello ","World!")
-- > splitAt 3 [1,2,3,4,5] == ([1,2,3],[4,5])
-- > splitAt 1 [1,2,3] == ([1],[2,3])
-- > splitAt 3 [1,2,3] == ([1,2,3],[])
-- > splitAt 4 [1,2,3] == ([1,2,3],[])
-- > splitAt 0 [1,2,3] == ([],[1,2,3])
-- > splitAt (-1) [1,2,3] == ([],[1,2,3])
--
-- It is equivalent to @('take' n xs, 'drop' n xs)@ when @n@ is not @_|_@
-- (@splitAt _|_ xs = _|_@).
-- 'splitAt' is an instance of the more general 'Data.List.genericSplitAt',
-- in which @n@ may be of any integral type.

-- the spec
splitAt :: Int -> [a] -> ([a],[a])
splitAt i xs = (take i xs, drop i xs)

-- the impl
splitAt' :: Int -> [a] -> ([a],[a])
splitAt' n xs
       |n <=0 			=  ([],xs)   --wenn n kleiner/gleich  als 0
	   |n >= length(xs)	=  (xs,[])   --wenn größer
	   |otherwise		= splitAtH' 1 n xs []   --ansonsten
	   
splitAtH' :: Int -> Int -> [a] -> [a] -> ([a],[a])	   
splitAtH' i n (x:xs) (ys)	= if i==n then (reverse(x:ys), xs) else splitAtH' (i+1) n xs (x:ys)
	   


------------------------------------------

-- | 'intercalate' inserts the list @xs@ in between
-- the lists in @xss@ and concatenates the
-- result.

-- 1. impl: direct or with map
intercalate :: [a] -> [[a]] -> [a]
intercalate (xxs) (y:ys) = if length(ys)>0 then y++xxs ++ (intercalate xxs ys ) else y
   

--concat = foldr (++) []
-- 2. impl: with foldr
-- after chapter about folds


intercalate' :: [a] -> [[a]] -> [a]
intercalate' l = foldr (\xs xss -> xs ++ l ++ xss) [] 

--foldr op e (x : xs) =  x `op` (foldr op e xs)
 
-- ----------------------------------------

-- | The 'partition' function takes a predicate and a list and returns
-- the pair of lists of elements which do and do not satisfy the
-- predicate, respectively; i.e.,
--

-- the spec
partition :: (a -> Bool) -> [a] -> ([a], [a])
partition p xs
  = (filter p xs, filter (not . p) xs)

-- 1. impl: direct
partition' :: (a -> Bool) -> [a] -> ([a], [a])
partition' p [] = ([],[])
partition' p (x:xs) = if (p x) then (x:lList, rList) else (lList, x:rList)
						where 
						(lList, rList) = partition' p xs

				
-- 2. impl: with foldr
-- after chapter about folds

partition'' :: (a -> Bool) -> [a] -> ([a],[a])
partition'' p = foldr teiler([],[])
					where 
						teiler xs (lList,rList)
							| p xs      = (xs:lList, rList)
							| otherwise = (lList, xs:rList)

-- ----------------------------------------
--
-- | all prefixes of a list
      
-- 1. impl: direct

inits        :: [a] -> [[a]]
inits [] = [[]]
inits (x:xs) =  [x] : map (x:) (inits xs)

-- 2. impl: with foldr
-- after chapter about folds

inits'        :: [a] -> [[a]]
inits' = foldr (\x xs ->[x] : map (x:) xs) [[]]
-- ----------------------------------------

-- | concatenates 2 lists of strings
-- with a given char in between the elements
--
-- the following law must hold for split and join
--
--   join' c (split' c xs) == xs
--
--   join' c . split c == id
--

join' :: a -> [[a]] -> [a]
join' c xxs = intercalate [c] xxs



-- | splits the input into sublists at delimiter
--   1. arg is the delimiter
--   the delimiter does not occur in elements of result list

split' :: Eq a => a -> [a] -> [[a]]
split' c [] = [[]]
split' c (x:xs)
        | x == c = [] : split' c xs
        | otherwise = [[x] ++ y] ++ ys
           where
              (y:ys) = split' c xs 
    
-- ----------------------------------------
