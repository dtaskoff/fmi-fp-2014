module ExprParserSolved where


import Parser
import ExprSolved
import Control.Applicative hiding (Const)
import Control.Monad (forever)


bool :: Parser Expr
bool = Const . boolean  <$> (char 'T' <|> char 'F')
    where boolean 'T' = True
          boolean 'F' = False

negation :: Parser Expr
negation = Not <$> (char '~' *> expr)

conjunction :: Parser Expr
conjunction = And <$> expr <*> (char '&' *> expr)

disjunction :: Parser Expr
disjunction = Or <$> expr <*> (char 'v' *> expr)

implication :: Parser Expr
implication = Ift <$> expr <*> (char '=' *> char '>' *> expr)

biconditional :: Parser Expr
biconditional = Iff <$> expr
                    <*> (char '<' *> char '=' *> char '>' *> expr)

-- write a parser for a logic expression, which is defined as follows:
-- * T and F are expressions, standing for True and False
-- * if E is an expression, then (~E) is an expression too
-- * if E1 and E2 are expressions, then:
-- ** (E1&E2)
-- ** (E1vE2)
-- ** (E1=>E2)
-- ** (E1<=>E2)
-- are all expressions too
expr :: Parser Expr
expr = inBraces negation
     <|> inBraces conjunction
     <|> inBraces disjunction
     <|> inBraces implication
     <|> inBraces biconditional
     <|> bool

-- implement a function which accepts a string, parses
-- it and returns the corresponding Expr data
expression :: String -> Maybe Expr
expression = fmap fst . parse expr

-- after the expr, expression and the eval (from the Expr module)
-- functions are written, you can test
-- all you have done with simply running
-- > runhaskell ExprParser
-- from the command-line or with
-- > main
-- from GHCi

evaluation :: String -> String
evaluation s = case expression s of
               Nothing -> "Bad expression format!"
               Just e  -> show $ eval $ e

main = do
    forever $ getLine >>= putStrLn . evaluation
