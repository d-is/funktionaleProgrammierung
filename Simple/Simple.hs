module Simple
where

-- Definieren Sie eine Funktion fib zur Berechung der Fibonacci-Zahlen
-- ab 0 
fib     :: Integer -> Integer
fib x 
  | x >= 2      = fib (x-1) + fib (x-2)
  | x == 1     = 1
  | x == 0      = 0




-- Definieren Sie eine Funktion fib zur Berechung der Fibonacci-Zahlen
-- ab 0 mit linearer Laufzeit

fib2    :: Integer -> Integer
fib2 n = fibH n 0 1 

fibH :: Integer -> Integer -> Integer -> Integer
fibH n x0 x1
    | n == 0     = x0
    | otherwise     = fibH (n-1) x1 (x0+x1)


-- Definieren Sie eine Funktion c (für Collatz), die berechnet
-- wie viele Rekursionsschritte benötigt werden, um
-- eine natürliche Zahl n >= 1 auf 1 zu
-- reduzieren.
--
-- Folgende Reduktionsregel sind dabei anzuwenden: Wenn n gerade ist,
-- so wird n halbiert, wenn n ungerade ist, so wird n verdreifacht und um
-- 1 erhöht.
    
c       :: Integer -> Integer
c n = cHelp n 0
  
cHelp  :: Integer -> Integer -> Integer
cHelp n i
  | (n `mod` 2) == 0     = cHelp (n `div` 2) (i+1)
  | n == 1        = i
  | otherwise         = cHelp ((n*3)+1) (i+1)

-- Definieren Sie ein endrekurive Variante von c
    
c1      :: Integer -> Integer
c1 = c1' 0
	where
	c1' :: Integer -> Integer -> Integer
	c1' r n 
		| n == 1	= r 
		| n `mod`2 == 0 = c1' (r+1) (n `div`2)
		| otherwise		= c1' (r+1) (3*n + 1)


-- Definieren Sie eine Funktion cmax, die für ein
-- Intervall von Zahlen das Maximum der
-- Collatz-Funktion berechnet. Nutzen Sie die
-- vordefinierten Funkt min und max.

cmax    :: Integer -> Integer -> Integer
cmax lb ub
	| lb > ub 	= error "empty intervall"
	| lb == ub	= c lb
	| otherwise	= c lb `max` cmax (lb+1) ub


-- Definieren Sie eine Funktion imax, die für ein
-- Intervall von Zahlen das Maximum einer
-- ganzzahligen Funktion berechnet. Formulieren
-- Sie die obige Funktion cmax so um, dass sie mit imax arbeitet.

imax    :: (Integer -> Integer) -> Integer -> Integer -> Integer
imax f lb ub
     | lb > ub 		= error "empty intervall"
     | lb == ub 	= f lb
     | otherwise 	= imax f (lb + 1) ub `max` f lb


cmax1   :: Integer -> Integer -> Integer
cmax1
    = imax c1

-- Entwickeln Sie eine Funktion,
-- die die Position und den Wert bestimmt, an der
-- das Maximum angenommen wird.
-- Versuchen Sie, eine endrekursive Lösung zu finden
-- (mit einer lokalen Hilfsfunktion).

imax2   :: (Integer -> Integer) -> Integer -> Integer -> (Integer, Integer)
imax2 f lb ub = undefined

-- ----------------------------------------