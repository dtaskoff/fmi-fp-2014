compose2 :: (b -> c) -> (a -> b) -> (a -> c)
compose2 f g x = f (g x)

composeMult :: [(a -> a)] -> (a -> a)
composeMult (f:fs) = f . composeMult fs
composeMult _      = id

composeMult2 :: [a -> a] -> a -> a
composeMult2 = foldr (.) id

-- ld2 stands for least divisor starting from 2
ld2 :: Integer -> Integer
ld2 n = ldh 2
    where ldh k | k ^ 2 > n    = n
                | mod n k == 0 = k
                | otherwise    = ldh (k + 1)

-- a number is a prime number,
-- if it is equal to it's least divisor
-- (greater than 1) 
isPrime :: Integer -> Bool
isPrime n = n > 1 && ld2 n == n

sumOfSquaredPrimes :: Integer -> Integer -> Integer
sumOfSquaredPrimes from to =
    sum $ map (^2) $ filter isPrime [from..to]

-- the (basic) reverse polish notation calculator,
-- this time in haskell ^^
-- we assume that the expression will be correct,
-- i.e. in the form "1 2 + 3 - 2 + 4 *"
solveRPN :: String -> Integer
solveRPN = head . foldl calc [] . words
    where calc acc@(x:y:zs) c =
            case c of
            "+" -> x + y : zs
            "-" -> y - x : zs
            "*" -> x * y : zs
            _   -> read c : acc        
          calc acc c = read c : acc

fold :: (a -> b -> b) -> b -> [a] -> b
fold f v (x:xs) = f x (fold f v xs)
fold _ v _      = v

sum' :: [Integer] -> Integer
sum' = fold (+) 0

length' :: [a] -> Integer
length' = fold (\_ l -> l + 1) 0

sumAndLength :: [Integer] -> (Integer, Integer)
sumAndLength = fold (\x (n, l) -> (n + x, l + 1)) (0, 0)

-- and this was the most brainstorming one
dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' p = fst . fold f ([], [])
    where f x (ys, xs) = (if p x then ys else x:xs, x:xs)
