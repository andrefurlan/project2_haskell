{-# OPTIONS_GHC -F -pgmF htfpp #-}

import Test.Framework

import Crusher

test_t = do assertEqual "WWW-WW-------BB-BBB" (boardToStr [W,W,W,D,W,W,D,D,D,D,D,D,D,B,B,D,B,B,B])
            assertEqual "WWW-WW-------BB-BBB" (boardToStr [W,W,W,D,W,W,D,D,D,D,D,D,D,B,B,D,B,B,B])

test_t2 = do assertEqual "WWW-WW-------BB-BBB" (boardToStr [W,W,W,D,W,W,D,D,D,D,D,D,D,B,B,D,B,B,B])
             assertEqual "WWW-WW-------BB-BB" (boardToStr [W,W,W,D,W,W,D,D,D,D,D,D,D,B,B,D,B,B,B])

{- To run this test using the Haskell Unit Test framework,
   first compile the code using ghc -o tests Tests.hs
   and then execute it using tests.exe or .\tests

-}
runTests :: Bool
runTests =
    testsTrToBoard &&
    testboardToStr &&
    testcrusher &&
    testgenerateGrid &&
    testgameOver

testboardToStr :: Bool
testboardToStr =
    -- test cases
    boardToStr [W,W,W,D,W,W,D,D,D,D,D,D,D,B,B,D,B,B,B]
        == "WWW-WW-------BB-BBB" &&

    boardToStr [W,W,W,D,W,W,D,D,D,B,D,D,D,B,B,D,B,B,B]
        == "WWW-WW---B---BB-BBB"

testsTrToBoard :: Bool
testsTrToBoard =
    -- test cases
    sTrToBoard "WWW-WW-------BB-BBB"
        == [W,W,W,D,W,W,D,D,D,D,D,D,D,B,B,D,B,B,B]

testcrusher :: Bool
testcrusher =
    crusher ["W------------BB-BBB","----W--------BB-BBB","-W-----------BB-BBB"] 'W' 2 3
        == ["W------------BB-BBB","----W--------BB-BBB","-W-----------BB-BBB"]

testgenerateGrid :: Bool
testgenerateGrid =
    generateGrid 3 2 4 []
        == [       (0,0),(1,0),(2,0),
                (0,1),(1,1),(2,1),(3,1),
             (0,2),(1,2),(2,2),(3,2),(4,2),
                (0,3),(1,3),(2,3),(3,3),
                   (0,4),(1,4),(2,4)]

testgameOver :: Bool
testgameOver =
    True == True

main = htfMain htf_thisModulesTests