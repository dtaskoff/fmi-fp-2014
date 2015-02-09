skips :: [a] -> [[a]]
skips xs = map (everynth xs) [0..length xs - 1]
    where everynth xs i = case splitAt i xs of
                          (_, y:ys) -> y : everynth ys i
                          _         -> []


nodups :: Eq a => [a] -> [a]
nodups = foldr f []
    where f y xs@(x:_) | y == x    = xs
                       | otherwise = y:xs
          f x _ = [x]
