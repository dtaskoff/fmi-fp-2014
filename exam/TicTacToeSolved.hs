module TicTacToeSolved where


import Data.List (transpose)


-- X field, O field and empty field
data Mark = X | O | E deriving (Show, Read, Eq)

type Row = [Mark]

type Board = [Row]

-- write a function which takes a mark and a board (3x3)
-- and tells us if the player using this mark has won

-- Note: every function here must be a partial applied one
-- written in point-free style, i.e.
-- func :: Int -> [Int] -> Bool
-- func x = all (== x)
-- or it won't count as a solution!
-- (and it should be readable, no cheating allowed..)
-- again: use :t and Hoogle!

-- Hint: lambdas, (.)s and ($)s..
row :: Mark -> Board -> Bool
row m = or . map (all (== m))

col :: Mark -> Board -> Bool
col m = row m . transpose

diag1 :: Mark -> Board -> Bool
diag1 m = and . zipWith ($) (map (\i -> (== m) . (!! i) . (!! i)) [0..2]) . repeat

diag2 :: Mark -> Board -> Bool
diag2 m = diag1 m . reverse

hasWon :: Mark -> Board -> Bool
hasWon m = or . zipWith ($) (map ($ m) [row, col, diag1, diag2]) . repeat
