import Control.Applicative
import Data.Char
import qualified Data.Map as Map


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

type Variable = String
type Operator = Char -- *, +, -

data Expression = Literal Variable
                | Number Int
                | Composite Operator [Expression]
    deriving (Show, Read)

variable :: Parser Variable
variable = oneOrMore $ satisfy isAlpha

literal :: Parser Expression
literal = Literal <$> variable

integer :: Parser Int
integer = read <$> (oneOrMore $ satisfy isDigit)

number :: Parser Expression
number = Number <$> integer

operator :: Parser Operator
operator = char '+' <|> char '-' <|> char '*'

composite :: Parser Expression
composite = Composite <$> operator <*> oneOrMore expression

expression :: Parser Expression
expression = spaces *> literal <|> spaces *> number <|> spaces *> composite

makeExpression :: String -> Maybe Expression
makeExpression expr = fst <$> parse expression expr

eval :: Map.Map Variable Int -> Expression -> Maybe Int
eval values (Literal var)     = Map.lookup var values 
eval _      (Number n)        = Just n
eval values (Composite op es) = fmap (foldl1 oper) $ sequence $  map (eval values) es
    where oper = case op of
                 '+' -> (+)
                 '-' -> (-)
                 '*' -> (*)
                 _   -> undefined

evaluate :: Map.Map Variable Int -> Maybe Expression -> Maybe Int
evaluate _          Nothing = Nothing
evaluate values (Just expr) = eval values expr
