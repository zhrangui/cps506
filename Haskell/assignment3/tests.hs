module Assignment3Test where
    import Test.HUnit
    import Assignment3

    test1 = TestCase (assertEqual "for (foo 3)," "./assignment.hs" (foo "./assignment1.hs"))
    --test1 = TestCase (assertEqual "for (foo 3)," (1,2) (foo 3))
    tests = TestList [TestLabel "test1" test1]