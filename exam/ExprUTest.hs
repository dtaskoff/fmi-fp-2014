-- import Expr
import ExprSolved
import Test.HUnit


test1 = TestCase $ assertEqual "T" True (eval $ Const True)
test2 = TestCase $ assertEqual "F" False (eval $ Const False)
test3 = TestCase $ assertEqual "TvF" True
            (eval $ Or (Const True) (Const False))
test4 = TestCase $ assertEqual "T&F" False
            (eval $ And (Const True) (Const False))
test5 = TestCase $ assertEqual "T<=>F" False
            (eval $ Iff (Const True) (Const False))
test6 = TestCase $ assertEqual "F=>T" True
            (eval $ Ift (Const False) (Const True))
test7 = TestCase $ assertEqual "T=>F" False
            (eval $ Ift (Const True) (Const False))
test8 = TestCase $ assertEqual "(~TvF)<=>(T=>F)" True
            (eval $ Iff (Or (Not (Const True)) (Const False))
                        (Ift (Const True) (Const False)))
test9 = TestCase $ assertEqual "(~T&~F)<=>(T<=>F)" True
            (eval $ Iff (And (Not (Const True)) (Not (Const False)))
                        (Iff (Const True) (Const False)))
test10 = TestCase $ assertEqual "(~F&~F)<=>(T<=>F)" False
            (eval $ Iff (And (Not (Const False)) (Not (Const False)))
                        (Iff (Const True) (Const False)))

tests = TestList [test1, test2, test3, test4,
            test5, test6, test7, test8, test9, test10]


main = runTestTT tests
