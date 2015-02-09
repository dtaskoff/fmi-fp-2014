module ExprSolved where


data Expr = Const Bool
          | Not Expr      -- negation
          | And Expr Expr -- conjunction
          | Or Expr Expr  -- disjunction
          | Ift Expr Expr -- implication
          | Iff Expr Expr -- biconditional
    deriving (Show, Read, Eq)


-- implement the following function, returning
-- True or False, depending on the expression
eval :: Expr -> Bool
eval (Const False) = False
eval (Const True)  = True
eval (Not x)       = not $ eval x
eval (Or x y)      = eval x || eval y
eval (And x y)     = eval x && eval y
eval (Ift x y)     = not (eval x) || eval y
eval (Iff x y)     = eval x == eval y
