and' :: [Bool] -> Bool
and' (x:xs) = x && and' xs
and' _      = True

or' :: [Bool] -> Bool
or' (x:xs) = x || or' xs
or' _      = False

all' :: (a -> Bool) -> [a] -> Bool
all' p (x:xs) =  p x && all' p xs
all' _ _      = True

any' :: (a -> Bool) -> [a] -> Bool
any' p (x:xs) =  p x || any' p xs
any' _ _      = False

isInfixOf' :: Eq a => [a] -> [a] -> Bool
isInfixOf' (x:xs) (y:ys) = x == y && isInfixOf' xs ys
isInfixOf' [] _          = True
isInfixOf' _  _          = False

lines' :: String -> [String]
lines' str = func [] [] str
    where func curr res (s:ss)
            | s == '\n' && not (null curr) = func [] (res ++ [curr]) ss
            | s == '\n'                    = func [] res ss
            | otherwise                    = func (curr ++ [s]) res ss
          func [] res [] = res
          func cr res [] = res ++ [cr]

words' :: String -> [String]
words' str = func [] [] str
    where func curr res (s:ss)
            | isWhitespace s && not (null curr) = func [] (res ++ [curr]) ss
            | isWhitespace s                    = func [] res ss
            | otherwise                         = func (curr ++ [s]) res ss
          func [] res [] = res
          func cr res [] = res ++ [cr]
          isWhitespace = (`elem` ['\n', ' ', '\t'])

map' :: (a -> b) -> [a] -> [b]
map' f (x:xs) = f x : map' f xs
map' _ _      = []

filter' :: (a -> Bool) -> [a] -> [a]
filter' p (x:xs) = if p x
                   then x : filter' p xs
                   else filter' p xs
filter' _ _      = []

foldl'' :: (a -> b -> a) -> a -> [b] -> a
foldl'' f v (x:xs) = foldl'' f (f v x) xs
foldl'' _ v _      = v

foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' f v (x:xs) = f x $ foldr' f v xs
foldr' _ v _      = v

-- doesn't work with infinite lists!
-- note:
-- complexity is O(max(length xs, length ys))
-- while previously was O(min(..., ...))
isInfixOf'' :: Eq a => [a] -> [a] -> Bool
isInfixOf'' xs ys | length xs > length ys = False
                  | otherwise = foldl (\acc (x, y) -> acc && x == y) True $ zip xs ys


-- brainstorming ^^
-- give some thoughts on that!
anyl :: [Bool] -> Bool
anyl xs = foldl (\acc x -> acc || x) False xs

anyr :: [Bool] -> Bool
anyr xs = foldr (\x acc -> x || acc) False xs

-- now we have repeat True
-- which makes an infinite lists of Trues
-- [True, True, True, True, ..]
-- test in ghci:
-- let trues = repeat True
-- anyl trues (press Ctrl-C)
-- anyr trues

-- what happens there and why?
