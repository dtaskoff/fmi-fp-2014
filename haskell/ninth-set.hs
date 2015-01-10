{-# LANGUAGE FlexibleInstances #-}

import System.Environment (getArgs)
import Data.Monoid


-- our simple implementation of the wc command
wc :: String -> String -> Int
wc input option =
    case option of
    "-l" -> length $ lines input
    "-w" -> length $ words input
    "-c" -> length input

main = do
    (option:_) <- getArgs
    input <- getContents
    print $ wc input option


data Animal = Cat | Dog | Duck
    deriving (Show, Read)

type Saying = String

cute :: Animal -> Saying
cute Cat   = "mew"
cute Dog   = "woof"
cute Duck  = "quack"

angry :: Animal -> Saying
angry Cat   = "hiss"
angry Dog   = "arf"
angry Duck  = "quack"

type Name  = String
type Phone = String

-- see how with the record syntax we don't need explicit
-- implementation of the name and the phone functions
data Person = Person { name  :: Name
                     , phone :: Phone
    } deriving (Show, Read, Eq, Ord)


-- our (partial) implementation of the Maybe datatype
data Shell a = Empty | A a
    deriving (Read)

instance Show a => Show (Shell a) where
    show Empty     = "Empty"
    show (A a) = "A " ++ show a

instance Eq a => Eq (Shell a) where
    (A a) == (A b) = a == b
    _     == _     = False

instance Ord a => Ord (Shell a) where
    compare (A a) (A b) = compare a b
    compare Empty (A _) = LT
    compare _     Empty = GT


data Tree a = Nil | Branch a (Tree a) (Tree a)
    deriving (Show, Read)

empty :: Tree a -> Bool
empty Nil   = True
empty _     = False

root :: Tree a -> Maybe a
root (Branch a _ _) = Just a
root _              = Nothing

left, right :: Tree a -> Maybe (Tree a)
left  (Branch _ l _)  = Just l
left  _               = Nothing
right (Branch _ _ r)  = Just r
right _               = Nothing

insert :: Ord a => a -> Tree a -> Tree a
insert a Nil    = Branch a Nil Nil
insert a (Branch v l r)
    | a < v     = Branch v (insert a l) r
    | otherwise = Branch v l (insert a r)


-- Beware! Monoids Follow!

newtype Product' a = Product' { getProduct' :: a }
    deriving (Show, Read, Eq, Ord, Bounded)

instance Num a => Monoid (Product' a) where
    mempty                              = Product' 1
    (Product' a) `mappend` (Product' b) = Product' (a * b)


data List m a = NilList
              | Entry m a
              | Append m (List m a) (List m a)
    deriving (Show, Read, Eq)

value :: Monoid m => List m a -> m
value NilList        = mempty
value (Entry m _)    = m
value (Append m _ _) = m

(+++) :: Monoid m => List m a -> List m a -> List m a
l1 +++ NilList = l1
NilList +++ l2 = l2
l1 +++ l2    = Append v l1 l2
    where v = (value l1) `mappend` (value l2)

toList :: List m a -> [a]
toList NilList          = []
toList (Entry _ a)      = [a]
toList (Append _ l1 l2) = toList l1 ++ toList l2

class Sized a where
    size :: a -> Integer

instance Sized (Sum Integer) where
    size (Sum n) = n

(!!?) :: (Monoid m, Sized m) => List m a -> Integer -> Maybe a
_                !!? n | n < 0     = Nothing
(Entry _ a)      !!? 0             = Just a
(Append _ l1 l2) !!? n | n < s     = l1 !!? n
                       | otherwise = l2 !!? (n-s)
                    where s = size $ value l1
_ !!? _ = Nothing


exampleList :: List (Sum Integer) Integer
exampleList = (Append (Sum 4)
                      (Entry (Sum 1) 42)
                      (Append (Sum 3) 
                              (Append (Sum 2)
                                      (Entry (Sum 1) 32)
                                      (Entry (Sum 1) 22))
                              (Entry (Sum 1) 12)))

-- Here be dragons! Functors!

-- the following will give you error if you try to run it, because
-- these instances are already implemented in haskell

-- instance Functor [] where
--    fmap _ []     = []
--    fmap f (x:xs) = f x : fmap xs
--    -- or simply fmap = map

-- instance Functor Maybe where
--    fmap _ Nothing  = Nothing
--    fmap f (Just x) = Just (f x)

-- instance Functor (Either a) where
--    fmap f (Left l)  = Left l
--    fmap f (Right r) = Right (f r)

instance Functor Tree where
    fmap _ Nil = Nil
    fmap f (Branch v l r) = Branch (f v) (fmap f l) (fmap f r)
