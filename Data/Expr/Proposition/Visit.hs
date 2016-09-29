-- | the visitor pattern for Expr

module Data.Expr.Proposition.Visit where

import Data.Expr.Proposition.Types

-- ----------------------------------------

data Visitor r
  = V { vLit    :: Bool  -> r
      , vVar    :: Ident -> r
      , vUnary  :: Op1   -> r -> r
      , vBinary :: Op2   -> r -> r -> r
      }

idExpr :: Visitor Expr
idExpr = V {vLit = Lit, vVar = Var . id, vUnary = Unary , vBinary = Binary . id}

visit :: Visitor r -> Expr -> r
visit vi = visit'
    where
        visit' (Lit b) = (vLit vi) b
        visit' (Var v) = (vVar vi) v
        visit' (Unary f e) = (vUnary vi) f (visit' e)
        visit' (Binary f e1 e2) = (vBinary vi) f (visit' e1) (visit' e2)

-- ----------------------------------------
