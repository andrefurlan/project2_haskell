import Test.HUnit

import Crusher

-- example
{-  To run this test using the Haskell Unit Test framework,
    first compile the code using ghci and use the command
    runTestTT tests
-}

test1 = TestCase (
    assertEqual
        "aaa" -- annotation
        (boardToStr [W,W,W,D,W,W,D,D,D,D,D,D,D,B,B,D,B,B,B]) -- function call
        "WWW-WW-------BB-BBB") -- expected output


test_gameOver = TestCase (
    assertEqual
        "Board present in list"
        (gameOver
            [W,W,W,D,W,W,D,D,D,D,D,D,D,B,B,D,B,B,B]
            [[W,B,W,D,W,W,D,D,D,D,D,D,D,B,B,D,B,B,B],[W,W,W,D,W,W,D,D,D,D,D,D,D,B,B,D,B,B,B],[W,W,W,D,W,W,D,D,D,D,D,D,D,B,B,D,B,B,B]]
            3)
        False)


gameOver_tests = TestList [test_gameOver]
tests = TestList [TestLabel "gameOver" gameOver_tests]