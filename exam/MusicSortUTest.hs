-- import MusicSort
import MusicSortSolved
import Test.HUnit


test1 = TestCase $ assertEqual "XXX OOO OOO" True
        $ hasWon X [[X, X, X], [O, O, O], [O, O, O]]
test2 = TestCase $ assertEqual "OOO XXX OOO" True
        $ hasWon X [[O, O, O], [X, X, X], [O, O, O]]
test3 = TestCase $ assertEqual "OXO OXX OXO" True
        $ hasWon X [[O, X, O], [O, X, X], [O, X, O]]
test4 = TestCase $ assertEqual "XOO OXO OOX" True
        $ hasWon X [[X, O, O], [O, X, O], [O, O, X]]
test5 = TestCase $ assertEqual "XOX OXO OXO" False
        $ hasWon X [[X, O, X], [O, X, O], [O, X, O]]
test6 = TestCase $ assertEqual "XOX OOO XOX" False
        $ hasWon X [[X, O, X], [O, O, O], [X, O, X]]
test7 = TestCase $ assertEqual "EEE OOO OEE" False
        $ hasWon O [[E, E, E], [E, O, O], [O, E, E]]
test8 = TestCase $ assertEqual "XOX EEE OOO" True
        $ hasWon O [[X, O, X], [E, E, E], [O, O, O]]
test9 = TestCase $ assertEqual "EEO EOE OEX" True
        $ hasWon O [[E, E, O], [E, O, E], [O, E, X]]
test10 = TestCase $ assertEqual "EXX EXO XOO" True
        $ hasWon X [[E, X, X], [E, X, O], [X, O, O]]

tests = TestList [test1, test2, test3, test4,
            test5, test6, test7, test8, test9, test10]


main = runTestTT tests
