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

-- Hint: lambdas, (.)s, ($)s
-- and check the functions from Data.List!
hasWon :: Mark -> Board -> Bool
