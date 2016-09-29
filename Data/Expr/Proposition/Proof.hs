module Data.Expr.Proposition.Proof where

import           Data.Expr.Proposition.Constr
import           Data.Expr.Proposition.Eval
import           Data.Expr.Proposition.Substitute
import           Data.Expr.Proposition.Types
import           Data.Pretty

import           Data.List                        (intercalate)
import           Data.Maybe                       (listToMaybe)

-- ----------------------------------------

truthTable :: Int -> [[Bool]]
truthTable n
	|n==0		= 	[[]]
	|n>0		=  [False: x | x <- t']++[True: x | x <- t'] where t' = truthTable (n-1)

-- compute a proof by generating a truth table,
-- iterate over all rows in the table
-- and substitute all variable by the values in a row
-- and evaluate the expression
-- if a single result is false
-- we have a counter example, else the expr
-- is a tautology

proof' :: Expr -> Maybe VarEnv
proof' e = proof'' table e
    where
        proof'' [] _ = Nothing -- Tauto
        proof'' (x:xs) e 
        
            | not (eval (substVars env e)) = Just env
            | otherwise = proof'' xs e
            where
                env = zip vars (map (Lit . id) x)
 
        vars = freeVars e
        table = truthTable (length vars)


proof :: Expr -> String
proof e
  = case proof' e of
     Nothing
       -> pretty e ++ " is a tautology"
     Just env
       -> pretty e ++ " isn't a tautology, " ++
          "a counter example is " ++ ppEnv env
  where
    ppEnv = intercalate ", " . map ppVar
    ppVar (i, v) = i ++ "=" ++ pretty v

-- ----------------------------------------
