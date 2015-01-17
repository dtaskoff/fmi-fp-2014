import Control.Applicative
import Data.Char

data Shell b = Empty | A b
    deriving (Show, Read, Eq, Ord)

instance Functor Shell where
    fmap _ Empty = Empty
    fmap f (A x) = A $ f x

instance Applicative Shell where
    pure        = A
    A f <*> A x = A $ f x
    _   <*> _   = Empty


-- here begins our applicative parser journey
type Word = String

data Sentence = Declaration [Word]
              | Question [Word]
              | Exclamation [Word]
    deriving (Show, Read)
               
newtype Parser a = Parser { parse :: String -> Maybe (a, String) }


satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f
    where f ""  = Nothing
          f (x:xs) | p x       = Just (x, xs)
                   | otherwise = Nothing

char :: Char -> Parser Char
char = satisfy . (==)

type Ending = Char

questionMark :: Parser Ending
questionMark = char '?'

exclamationPoint :: Parser Ending
exclamationPoint = char '!'

period :: Parser Ending
period = char '.'

space :: Parser Char
space = char ' '

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

spaces :: Parser String
spaces = zeroOrMore space

word :: Parser Word
word = oneOrMore $ satisfy isAlpha

-- a generic sentence parser
sentence :: Parser Ending -> Parser [Word]
sentence ending = zeroOrMore (spaces *> word) <* ending

-- anothing variation of our sentence parser, appending the ending character to our Sentence

-- we use this function only to append
-- the ending character to our list of words
append :: [Word] -> Ending -> [Word]
append word c = word ++ [[c]]

sentence2 :: Parser Ending -> Parser [Word]
sentence2 ending = append <$> zeroOrMore (spaces *> word) <*> ending

-- making a specific parser for every sentence type
declaration :: Parser Sentence
declaration = Declaration <$> (sentence period)

question :: Parser Sentence
question = Question <$> (sentence questionMark)

exclamation :: Parser Sentence
exclamation = Exclamation <$> (sentence exclamationPoint)

-- and this is our final sentence parser!
reader :: Parser Sentence
reader = declaration <|> question <|> exclamation

readASentence :: String -> Maybe Sentence
readASentence = fmap fst . parse reader
