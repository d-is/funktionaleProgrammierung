{-# LANGUAGE DeriveDataTypeable #-}

-- ----------------------------------------

-- | binary tree with values at the leafs (Tip),
-- the branches (Bin) don't contain any further information,
-- the empty tree is represented by a special value Null

module Data.Tree
where

import           Prelude             hiding (foldl, foldr, head, tail, init, last)

import           Control.Applicative
import           Control.Monad

import           Data.Data
import           Data.Foldable
import           Data.Monoid

-- ----------------------------------------

data Tree a
    = Null
    | Tip a
    | Bin (Tree a) (Tree a)
      deriving (Show, Data, Typeable)

-- | data type invariant

invTree :: Tree a -> Bool
invTree Null = True
invTree (Tip x) = True
invTree (Bin Null _) = False
invTree (Bin _ Null) = False
invTree (Bin l r) = invTree l && invTree r

-- | smart constructor
bin :: Tree a -> Tree a -> Tree a
bin Null b = b
bin a Null = a
bin a b    = Bin a b

instance Functor Tree where
  fmap f Null = Null
  fmap f (Tip x) = Tip (f x)
  fmap f (Bin l r) = Bin (fmap f l)(fmap f r)

instance Applicative Tree where
  pure  = undefined
  (<*>) = undefined

instance Monad Tree where
  return     = undefined
  _    >>= _ = undefined

instance Alternative Tree where
  empty = mzero   
  (<|>) = mplus

instance MonadPlus Tree where
  mzero = undefined
  mplus = undefined

instance Monoid (Tree a) where
  mempty  = undefined
  mappend = undefined

-- fold elements like in a list from right to left
instance Foldable Tree where
	foldr _ e Null = e
	foldr f e (Tip x) = f x e
	foldr f e (Bin l r) = foldr f (foldr f e r) l

-- ----------------------------------------
-- classical visitor

visitTree :: b -> (a -> b) -> (b -> b -> b) -> Tree a -> b
visitTree fnill ftip fbin  = visit'
  where
    visit' Null      = fnill
    visit' (Tip a)   = ftip a
    visit' (Bin l r) = visit' l `fbin` visit' r  

-- special visitors

sizeTree :: Tree a -> Int
sizeTree = visitTree 0 (\x -> 1) (+)

minDepth, maxDepth :: Tree a -> Int
minDepth = visitTree 0 (\x -> 1) (\x y -> 1 + min x y  )
maxDepth = visitTree 0 (\x -> 1) (\x y -> 1 + max x y  )
-- ----------------------------------------
-- access functions

viewL :: Tree a -> Maybe (a, Tree a)
viewL Null           = Nothing
viewL (Tip x)       = Just (x, Null)    -- sobald tiefstes linkes Element (Tip) erreicht, bricht er rekursion ab und gibt den wert hoch
viewL (Bin l r) = Just (x, (bin xs r)) --endgültige Rückgabe, bestehend aud dem gewüschten element und durch smart constructor erstellten neuen baum aus xs (alle linken bins mit werten) sowie allen rechten un berührten knoten r
         where 
           (Just (x, xs)) = viewL l --aus den ergebis von der Rekursion (viewL l) erstellt er die Rückgabe Just(x, xs), wobei xs null ist

viewR :: Tree a -> Maybe (Tree a, a)
viewR Null           = Nothing
viewR (Tip x)       = Just (Null, x)  
viewR (Bin l r) = Just ((bin l xs), x) 
         where 
           (Just (xs, x)) = viewR r 


head :: Tree a -> a
head = maybe (error "head: empty tree") fst . viewL

tail :: Tree a -> Tree a
tail = maybe (error "tail: empty tree") snd . viewL

last :: Tree a -> a
last = maybe (error "last: empty tree") snd . viewR

init :: Tree a -> Tree a
init = maybe (error "init: empty tree") fst . viewR

-- ----------------------------------------
-- conversions to/from lists

-- | runs in O(n) due to the use of (:)
toList :: Tree a -> [a]
toList = foldr (:) []

-- | runs in O(n^2) due to the use of (++)
toListSlow :: Tree a -> [a]
toListSlow = visitTree [] (\x -> [x]) (\x y -> x ++ y )

-- | build a balanced tree
--
-- doesn't work for infinite lists

-- strong balancing criterion
fromList :: [a] -> Tree a
fromList [] = Null
fromList [x]=  Tip x
fromList xs = Bin (fromList l) (fromList r)  
			where 
			(l,r) = splitAt (length xs `div` 2) xs 



-- weak balancing criterion

fromList' :: [a] -> Tree a
fromList' [] = Null
fromList' [x] = Tip x
fromList' (x:xs) = bin (fromList xs) (Tip x)


-- list to the right
fromList'' :: [a] -> Tree a
fromList'' = foldr (\ x t -> Tip x `bin` t) Null

-- list to the left
fromList''' :: [a] -> Tree a
fromList''' = foldl (\ t x -> t `bin` Tip x) Null

-- runtime differences between fromList, fromList', fromList'', fromList'''?
-- differences in balancing quality?

-- ----------------------------------------
