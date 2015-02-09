-- import ExprParser hiding (main)
import ExprSolved
import Parser
import ExprParserSolved hiding (main)
import Test.HUnit


test1 = TestCase $ assertEqual "T" (Just (Const True, ""))
            $ parse expr "T"
test2 = TestCase $ assertEqual "F" (Just (Const False, ""))
            $ parse expr "F"
test3 = TestCase $ assertEqual "(TvF)"
            (Just (Or (Const True) (Const False), ""))
            (parse expr "(TvF)")
test4 = TestCase $ assertEqual "(T&F)"
            (Just (And (Const True) (Const False), ""))
            (parse expr "(T&F)")
test5 = TestCase $ assertEqual "(T<=>F)"
            (Just (Iff (Const True) (Const False), ""))
            (parse expr "(T<=>F)")
test6 = TestCase $ assertEqual "(F=>T)"
            (Just (Ift (Const False) (Const True), ""))
            (parse expr "(F=>T)")
test7 = TestCase $ assertEqual "(T=>F)"
            (Just (Ift (Const True) (Const False), ""))
            (parse expr "(T=>F)")
test8 = TestCase $ assertEqual "(((~T)vF)<=>(T=>F))"
            (Just (Iff (Or (Not (Const True)) (Const False))
                 (Ift (Const True) (Const False)), ""))
            (parse expr "(((~T)vF)<=>(T=>F))")
test9 = TestCase $ assertEqual "(((~T)&(~F))<=>(T<=>F))"
            (Just (Iff (And (Not (Const True)) (Not (Const False)))
                 (Iff (Const True) (Const False)), ""))
            (parse expr "(((~T)&(~F))<=>(T<=>F))")
test10 = TestCase $ assertEqual "((~F&~F)<=>(T<=>F))"
            (Just (Iff (And (Not (Const False)) (Not (Const False)))
                        (Iff (Const True) (Const False)), ""))
            (parse expr "(((~F)&(~F))<=>(T<=>F))")

test11 = TestCase $ assertEqual "(T)" Nothing (parse expr "(T)")
test12 = TestCase $ assertEqual "~F" Nothing (parse expr "~F")
test13 = TestCase $ assertEqual "(~T<=>F)" Nothing
           (parse expr "(~T<=>F)")
test14 = TestCase $ assertEqual "(((~T)vF)<=>(T=>F))"
            (Just $ Iff (Or (Not (Const True)) (Const False))
                 (Ift (Const True) (Const False)))
            (expression "(((~T)vF)<=>(T=>F))")
test15 = TestCase $ assertEqual "(((~T)&(~F))<=>(T<=>F))"
            (Just $ Iff (And (Not (Const True)) (Not (Const False)))
                 (Iff (Const True) (Const False)))
            (expression "(((~T)&(~F))<=>(T<=>F))")

test16 = TestCase $ assertEqual "(T<=>(~T))" "False"
            $ evaluation "(T<=>(~T))"
test17 = TestCase $ assertEqual "(T=>(((~T)=>T)<=>(T=>T)))" "True"
            $ evaluation "(T=>(((~T)=>T)<=>(T=>T)))"
test18 = TestCase $ assertEqual "(T<=>~F)" "Bad expression format!"
            $ evaluation "(T<=>~F)"
test19 = TestCase $ assertEqual "(T<=>F" "Bad expression format!"
            $ evaluation "(T<=>F"
test20 = TestCase $ assertEqual "(((~F)vT)<=>((~F)&T))" "True"
            $ evaluation "(((~F)vT)<=>((~F)&T))"


tests = TestList [test1, test2, test3, test4,
            test5, test6, test7, test8, test9, test10,
            test11, test12, test13, test14, test15,
            test16, test17, test18, test19, test20]


main = runTestTT tests
