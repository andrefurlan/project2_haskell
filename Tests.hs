import Crusher

runTests :: Bool
runTests =
    testsTrToBoard &&
    testboardToStr

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
