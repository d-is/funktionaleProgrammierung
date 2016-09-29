module Data.FunctionalList
where

import           Prelude (Bool(..), (.), (++), undefined)
import qualified Prelude as P

type List a = [a] -> [a]

-- ----------------------------------------

fromList        :: [a] -> List a
fromList xs    = \ list -> xs ++ list  

toList          :: List a -> [a]
toList xs      = xs []

empty           :: List a
empty           = \x -> []

singleton       :: a -> List a
singleton x     = fromList [x]

-- (:) for functional lists
cons            :: a -> List a -> List a
cons x list        = \xs -> x : list xs 

-- dual to cons
snoc            :: List a -> a -> List a
snoc list x        = list `append`(singleton x)

-- (++) for functional lists
append          :: List a -> List a -> List a
append list1 list2    = fromList((toList list1)++(toList list2))


---  \xs -> list1 (list2 xs) 

-- like concat for normal lists: foldr (++) []
concat          :: [List a] -> List a
concat xs         = P.foldr append empty xs

-- like map for normal lists: foldr ((:) . f) []
map             :: (a -> b) -> List a -> List b
map f list          =   fromList( P.map f (toList list))

-- foldr with foldr for normal lists
foldr           :: (a -> b -> b) -> b -> List a -> b
foldr op n list     = P.foldr op n (toList list)

-- head, tail, null
head            :: List a -> a
head list           = P.head (toList list)

tail            :: List a -> List a
tail list           = fromList(P.tail (toList list))

null            :: List a -> Bool
null list           = P.null (toList list)

reverse         :: List a -> List a
reverse list        =  fromList (P.reverse (toList list))

-- ----------------------------------------
