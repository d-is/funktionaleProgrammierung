-- | the visitor pattern for Expr

module Data.Expr.Proposition.Visitors where

import Data.Expr.Proposition.Types
import Data.Expr.Proposition.Visit
import Data.Expr.Proposition.Eval (mf1, mf2)

import Data.Set(Set)
import qualified Data.Set as S

-- ----------------------------------------
type Idents = Set Ident

freeVarsVisitor :: Visitor Idents
freeVarsVisitor = V {vLit = const S.empty, vVar = S.singleton, vUnary = (\ f -> id), vBinary = (\ f -> S.union)}

freeVars :: Expr -> Idents
freeVars = visit freeVarsVisitor

type VarEnv = [(Ident, Expr)]


substVarsVisitor :: VarEnv -> Visitor Expr
substVarsVisitor env = V {vLit = Lit, vVar = (\i -> maybe (Var i) id (lookup i env)), vUnary = Unary, vBinary = Binary}

substVars :: VarEnv -> Expr -> Expr
substVars env = visit (substVarsVisitor env)

evalVisitor :: Visitor Bool
evalVisitor = V { vLit = id, 
                  vVar = (\x -> error "free variables"), 
                  vUnary = (\ op e -> (mf1 op) e), 
                  vBinary = (\ op e1 e2-> (mf2 op) e1 e2)
                  }

eval :: Expr -> Bool
eval = visit evalVisitor

-- ----------------------------------------
