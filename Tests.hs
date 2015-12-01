import Crusher

runTests :: Bool
runTests =
    testsTrToBoard &&
    testboardToStr &&
    testcrusher &&
    testgenerateGrid

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

