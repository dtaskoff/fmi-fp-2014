import Control.Applicative
import Data.Char
import qualified Data.Map as Map


-- create a program that reads a list of expressions (each expression is on a separate line) from a file,
-- reads a line with variables and their values from the keyboard
-- and then for each expression in the file outputs it's value calculated with the inputted values
-- if there is an invalid expression, output "Invalid"

-- Expample program flow:
-- in the file expressions.txt we have + 3 x - 4 y * 5 z

-- starting with:
-- $> runhaskell parser.hs expressions.txt
-- > x 3 y 2 z 1
-- > 3
-- some explanations: the expressions are calculated scheme-like, only without the parenthesis:
-- + 3 3 - 4 2 * 5 1 <=> (+ 3 3 (- 4 2 (* 5 1))) <=> (+ 3 3 (- 4 2 5)) <=> (+ 3 3 (-3)) <=> 3



newtype Parser a = Parser { parse :: String -> Maybe (a, String) }


satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f
    where f ""  = Nothing
          f (x:xs) | p x       = Just (x, xs)
                   | otherwise = Nothing

char :: Char -> Parser Char
char = satisfy . (==)

first :: (a -> b) -> (a, c) -> (b, c)
first f (x, y) = (f x, y)

-- making Parser an instance of Functor
-- will give us the ability to construct a new parser with
-- simply mapping a function over an existing parser
instance Functor Parser where
    fmap f (Parser p) = Parser $ fmap (first f) . p

-- creating a Parser instance for Applicative
-- will make it possible to chain parsers
instance Applicative Parser where
    pure a = Parser (\s -> Just (a, s))
    (Parser fp) <*> xp = Parser $ \s ->
         case fp s of
         Nothing      -> Nothing
         Just (f, s') -> parse (f <$> xp) s'

-- we are making also an Alternative instance
-- because we may want to get some results
-- without all parsers in the chain successing
instance Alternative Parser where
    empty                       = Parser $ const Nothing
    (Parser p1) <|> (Parser p2) = Parser $ (<|>) <$> p1 <*> p2


-- these are called 'some' and 'many' in the Control.Applicative module
zeroOrMore :: Parser a -> Parser [a]
zeroOrMore p = oneOrMore p <|> pure []

oneOrMore :: Parser a -> Parser [a]
oneOrMore p = (:) <$> p <*> zeroOrMore p

space :: Parser Char
space = char ' '

spaces :: Parser String
spaces = zeroOrMore space


-- here begin the actual tasks!
type Variable = String
type Operator = Char -- *, +, -

data Expression = Literal Variable
                | Number Int
                | Composite Operator [Expression]
    deriving (Show, Read)

-- create the following parsers:

-- parse variable "var + 1 2" -> Just ("var", " + 1 2")
-- parse variable "+ 1 2" -> Nothing
variable :: Parser Variable

-- parse literal "var + 1 2" -> Just (Literal "var")
literal :: Parser Expression

-- parse integer "var + 1 2" -> Nothing
-- parse integer "12"        -> Just (12, "")
integer :: Parser Int

-- parse number "12" -> Just (Number 12)
number :: Parser Expression

-- parse operator "+ 1 2" -> Just ('+', " 1 2")
operator :: Parser Operator

-- parse composite "+ 1 2 a" -> Just (Composite '+' [Number 1, Number 2, Literal a]
composite :: Parser Expression

-- parse expression "+ 1 - a 2" -> Just (Composite '+' [Number 1,
--                                                      Composite '-' [Literal "a", Number 2]])
expression :: Parser Expression

-- makeexpression "+ 1 - a 2"   -> the same as the above example
makeExpression :: String -> Maybe Expression

-- hint for the next functions:
-- there exists a function foldl1 which does the following:
-- foldl1 (-) [1..5] = 1 - 2 - 3 - 4 - 5 = -13
-- foldl1 (+) [1..3] = 1 + 2 + 3 = 6

-- eval Map.empty "+ 1 2 - 3 2" -> Just 2
-- eval (Map.fromList [("x", 2), ("y", 3)]) "+ x 2 - y 2" -> Just 3
eval :: Map.Map Variable Int -> Expression -> Maybe Int

-- the same as eval, except that it works with Maybe Expression (not with pure Expression)
evaluate :: Map.Map Variable Int -> Maybe Expression -> Maybe Int

main :: IO ()

-- you will need those:
-- Map.empty -> returns the empty map
-- Map.lookup k map-> if there exists a key 'k' in map, then (Just 'v') is returned, else Nothing
-- Map.fromList [(k1, v1), (k2, v2),..] -> creates a map with key-value pairs (k1,v1) and so on
