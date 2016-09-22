module Data.IntervalSet
where

-- ----------------------------------------

-- an pair of Ints can represent closed Intervals
-- (i, j) <=> [i..j]
-- Intervalls with i > j represent the empty set

type Interval = (Int, Int)

overlap :: Interval -> Interval -> Bool
overlap (x1, y1) (x2, y2)
  | x1 >= x2 && x1 <= y2 	= True
  | y1 <= y2 && y1 >= x2 	= True
  | otherwise 				= False
--überlappen sie oder stoßen sie an einander??

less :: Interval -> Interval -> Bool
less (_x1, y1) (x2, _y2)
    | y1 < x2       = True		-- Nur kleiner als, weil Überlappung nicht möglich
	| otherwise 	= False
--für die Ordnung
                           
emptyInterval :: Interval -> Bool
emptyInterval (x, y) = x>=y

--ist es empty??


-- merge 2 (overlapping) intervals
merge :: Interval -> Interval -> Interval
merge (x1, y1) (x2, y2)
			| x1 <= x2 && y1 >= y2		= (x1, y1)
			| x1 >= x2 && y1 <= y2		= (x2, y2)
			| x1 <= x2 && y1 < y2		= (x1, y2)
			| x1 > x2 && y1 > y2		= (x2, y1)
				
--baue überlappende zu einem tupel zusammen!

-- ----------------------------------------

-- a set of integers can be represented by an
-- ordered list of none empty intervals, which
-- do not overlap

type IntervalSet = [Interval]

inv :: IntervalSet -> Bool
inv [] = True
inv [x] = not (emptyInterval x)
inv (x1:x2:xs) = 
       not (emptyInterval x1) 
       &&
       not (overlap x1 x2)
       &&
       less x1 x2
       && 
       inv (x2:xs)


-- ----------------------------------------
-- internal interval set ops
--ein elemetige Intervalle

singleInterval :: Int -> Int -> IntervalSet
singleInterval x y
    | x <= y    = [(x, y)]
    | otherwise = []
--einfügen
insertInterval :: Interval -> IntervalSet -> IntervalSet
insertInterval (x, y) [] = singleInterval x y
insertInterval (x, y) (xs:xss)
            | overlap (x, y) xs = insertInterval (merge (x, y) xs) xss
            | less (x, y) xs = (x, y) : xs : xss
            | otherwise = xs : insertInterval (x, y) xss
				



--einfügen einer ganzen Interval Liste
fromIntervalList :: [(Int, Int)] -> IntervalSet
fromIntervalList []		= []
fromIntervalList (x:xs) = insertInterval(x)(fromIntervalList(xs))


-- ----------------------------------------
--
-- exported ops, names similar to Data.Set

empty :: IntervalSet
empty = []

singleton :: Int -> IntervalSet
singleton i = singleInterval i i

insert :: Int -> IntervalSet -> IntervalSet
insert i = insertInterval (i, i)

--Mengen vereinigung
union :: IntervalSet -> IntervalSet -> IntervalSet
union xs ys = fromIntervalList(xs++ys) 

member :: Int -> IntervalSet -> Bool
member x [] 	  = False
member x ((i,j):xss)
	| x < i = False
	| x <= j = True	
	| otherwise = member x xss
           
fromList :: [Int] -> IntervalSet
fromList = fromIntervalList . map (\ x -> (x,x))

toList :: IntervalSet -> [Int]
toList [] = []
toList ((x,y):xs) = [x..y] ++ toList xs



-- ----------------------------------------
