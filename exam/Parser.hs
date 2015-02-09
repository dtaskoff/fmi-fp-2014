module Parser ( Parser
              , parse
              , satisfy
              , char
              , inBraces)
    where 


import Control.Applicative 
import Data.Char (isAlpha)


-- You should already be familiar with what all that does
newtype Parser a =
    Parser { parse :: String -> Maybe (a, String) }

first :: (a -> b) -> (a, c) -> (b, c)
first f (x, y) = (f x, y)

instance Functor Parser where
    fmap f p = Parser $ fmap (first f) . parse p

instance Applicative Parser where
    pure a = Parser $ \s -> Just (a, s)
    Parser fp <*> Parser xp = Parser $ \s ->
        case fp s of
        Just (f, s') -> fmap (first f) $ xp s'
        _            -> Nothing

instance Alternative Parser where
    empty = Parser $ \s -> Nothing
    Parser l <|> Parser r = Parser $ \s -> l s <|> r s

satisfy :: (Char -> Bool) -> Parser Char
satisfy pred = Parser $
    \s -> case s of
         (c:cs) -> if pred c then Just (c, cs)
                            else Nothing
         _      -> Nothing

char :: Char -> Parser Char
char c = satisfy (== c)

openingBrace :: Parser Char
openingBrace = char '('

closingBrace :: Parser Char
closingBrace = char ')'

inBraces :: Parser a -> Parser a
inBraces p = openingBrace *> p <* closingBrace
