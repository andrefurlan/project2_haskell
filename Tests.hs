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


test_samemove1 = TestCase (
    assertEqual
        "Board present in list"
        (gameOver
            [W,W,W,D,W,W,D,D,D,D,D,D,D,B,B,D,B,B,B]
            [[W,B,W,D,W,W,D,D,D,D,D,D,D,B,B,D,B,B,B],[W,W,W,D,W,W,D,D,D,D,D,D,D,B,B,D,B,B,B],[W,W,W,D,W,W,D,D,D,D,D,D,D,B,B,D,B,B,B]]
            3)
        True)

test_samemove2 = TestCase (
    assertEqual
        "Board not present in list"
        (gameOver
            [W,W,W,D,W,W,D,D,D,D,D,D,D,B,B,D,B,B,B]
            [[W,B,W,D,W,W,D,D,D,D,D,D,D,D,B,D,B,B,B],[W,D,W,D,W,W,D,D,D,D,D,D,D,B,B,D,B,B,B],[D,W,W,D,W,W,D,D,D,D,D,D,D,B,B,D,B,B,B]]
            3)
        True)

test_fewW1 = TestCase (
    assertEqual
        "Too few W"
        (gameOver
            [D,W,D,D,D,W,D,D,D,D,D,D,D,B,B,D,B,B,B]
            [[W,B,W,D,W,W,D,D,D,D,D,D,D,B,B,D,B,B,B],[W,W,W,D,W,W,D,D,D,D,D,D,D,B,B,D,B,B,B],[W,W,W,D,W,W,D,D,D,D,D,D,D,B,B,D,B,B,B]]
            3)
        True)

test_fewW2 = TestCase (
    assertEqual
        "Equal W"
        (gameOver
            [W,D,D,D,W,W,D,D,D,D,D,D,D,B,B,D,B,B,B]
            [[W,B,W,D,W,W,D,D,D,D,D,D,D,B,B,D,B,B,B],[W,W,W,D,W,W,D,D,D,D,D,D,D,B,B,D,B,B,B],[W,W,W,D,W,W,D,D,D,D,D,D,D,B,B,D,B,B,B]]
            3)
        False)

test_fewW3 = TestCase (
    assertEqual
        "Too many W"
        (gameOver
            [W,W,W,D,W,W,D,D,D,D,D,D,D,B,B,D,B,B,B]
            [[W,B,W,D,W,W,D,D,D,D,D,D,D,B,B,D,B,B,B],[W,W,W,D,W,W,D,D,D,D,D,D,D,B,B,D,B,B,B],[W,W,W,D,W,W,D,D,D,D,D,D,D,B,B,D,B,B,B]]
            3)
        False)

gameOver_tests = TestList [test_samemove1, test_samemove2, test_fewW1, test_fewW2, test_fewW3]
tests = TestList [TestLabel "gameOver" gameOver_tests]