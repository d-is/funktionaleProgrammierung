module TestFunctionalMap
where

import           Data.FunctionalMap
import           Prelude            hiding (lookup)
import qualified Prelude            as P
import           Test.QuickCheck

--Ürüfe das hinzufügen eines Paares zur Map
prop_1 :: String -> Bool
prop_1 xs = if (lookup xs (insert xs xs empty)) == Just xs then True else False 

--Vergleiche ob nach hinzufügen und löschen eines Paares, das Paar wirklich entfernt wurde
prop_2 :: String -> Bool
prop_2 xs = if (lookup xs (delete xs(insert xs xs empty))) == Nothing then True else False 

--Vergleich zweier Maps, in welcher einerseits xs per insert hinzugefügt wurde und anderseits per fromList[(xs,xs)]
prop_3 :: String -> Bool
prop_3 xs = if (lookup xs (insert xs xs empty)) == lookup xs (fromList [(xs,xs)]) then True else False 


-- ----------------------------------------

quickCheck' :: Testable prop => prop -> IO ()
quickCheck' = quickCheckWith stdArgs{maxSuccess=200}

main
  = do quickCheck'  prop_1
       quickCheck'  prop_2
       quickCheck'  prop_3
       
-- ----------------------------------------

