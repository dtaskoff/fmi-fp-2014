module Expr where


data Expr = Const Bool
          | Not Expr      -- negation
          | And Expr Expr -- conjunction
          | Or Expr Expr  -- disjunction
          | Ift Expr Expr -- implication
          | Iff Expr Expr -- biconditional
    deriving (Show, Read)


-- implement the following function, returning
-- True or False, depending on the expression
eval :: Expr -> Bool
eval = undefined
