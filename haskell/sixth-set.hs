sum' :: Int -> Int -> Int
sum' x y = x + y

area :: Float -> Float -> Float -> Float
area a b c = sqrt (p2 * (p2 - a) * (p2 - b) * (p2 - c))
    where p2 = (a + b + c) / 2

isTriangle :: Float -> Float -> Float -> Bool
isTriangle a b c
    | a < 0 || b < 0 || c < 0 = False
    | a + b <= c || b + c <= a || c + a <= b = False
    | otherwise = True

correctArea :: Float -> Float -> Float -> Float
correctArea a b c = if isTriangle a b c
                    then area a b c
                    else 0

head' :: [a] -> a
head' (x:xs) = x

last' :: [a] -> a
last' [x] = x
last' (x:xs) = last' xs

tail' :: [a] -> [a]
tail' (x:xs) = xs
