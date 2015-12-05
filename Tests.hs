import Test.HUnit

import Crusher

-- example
{-  To run this test using the Haskell Unit Test framework,
    first compile the code using ghci and use the command
    runTestTT tests
-}


tests = TestList [TestLabel "gameOver" gameOver_tests, TestLabel "generateNewStates" generateNewState_tests]


-- Generate States test
generateNewState_tests = TestList [test_genState1]

hist :: [Board]
hist = [[W,W,W,D,W,W,D,D,D,D,D,D,D,B,B,D,B,B,B],
        [W,W,W,D,W,D,D,D,D,W,D,D,D,B,B,D,B,B,B],
        [W,W,W,D,W,D,D,D,D,W,D,D,B,B,B,D,D,B,B],
        [W,W,D,D,W,W,D,D,D,W,D,D,B,B,B,D,D,B,B],
        [W,W,D,D,W,W,D,D,D,B,D,D,B,D,B,D,D,B,B],
        [W,W,D,D,W,D,D,D,D,W,D,D,B,D,B,D,D,B,B],
        [W,W,D,D,W,D,D,D,D,B,D,D,B,D,B,D,D,B,D],
        [W,W,D,D,D,D,D,D,D,W,D,D,B,D,B,D,D,B,D],
        [W,W,D,D,D,D,D,D,D,W,D,D,B,B,B,D,D,D,D],
        [W,W,D,D,W,D,D,D,D,D,D,D,B,B,B,D,D,D,D],
        [W,W,D,D,W,D,D,D,D,B,D,D,B,D,B,D,D,D,D]]

cB :: Board
cB = [W,W,D,D,W,D,D,D,D,B,D,D,B,D,B,D,D,D,D]

gr :: Grid
gr = [(0,0),(1,0),(2,0),(0,1),(1,1),(2,1),(3,1),(0,2),(1,2),(2,2),(3,2),(4,2),(0,3),(1,3),(2,3),(3,3),(0,4),(1,4),(2,4)]
gr2 :: Grid
gr2 = [(0,0),(1,0),(0,1),(1,1),(2,1),(0,2),(1,2)]

jm :: [Jump]
jm = generateLeaps gr 3

sld :: [Slide]
sld = generateSlides gr 3

test_genState1 = TestCase (
    assertEqual
      "List of all possible new states"
      (generateNewStates cB hist gr sld jm W)
      [[D,W,W,D,W,D,D,D,D,B,D,D,B,D,B,D,D,D,D],
       [D,W,D,D,W,D,D,D,D,W,D,D,B,D,B,D,D,D,D],
       [W,D,D,D,W,D,D,D,W,B,D,D,B,D,B,D,D,D,D],
       [D,W,D,W,W,D,D,D,D,B,D,D,B,D,B,D,D,D,D],
       [W,D,W,D,W,D,D,D,D,B,D,D,B,D,B,D,D,D,D],
       [W,D,D,D,W,W,D,D,D,B,D,D,B,D,B,D,D,D,D],
       [W,W,D,W,D,D,D,D,D,B,D,D,B,D,B,D,D,D,D],
       [W,W,D,D,D,W,D,D,D,B,D,D,B,D,B,D,D,D,D],
       [W,W,D,D,D,D,D,D,W,B,D,D,B,D,B,D,D,D,D]])

test_genSlides3 = TestCase (
    assertEqual
        "List of all possible slides size 3"
        (generateSlides gr 3)
        [
            ((0,0),(0,1)),
            ((1,0),(1,1)),
            ((2,0),(2,1)),
            ((0,1),(0,2)),
            ((1,1),(1,2)),
            ((2,1),(2,2)),
            ((3,1),(3,2)),
            ((0,0),(1,1)),
            ((1,0),(2,1)),
            ((2,0),(3,1)),
            ((0,1),(1,2)),
            ((1,1),(2,2)),
            ((2,1),(3,2)),
            ((3,1),(4,2)),
            ((1,2),(0,3)),
            ((2,2),(1,3)),
            ((3,2),(2,3)),
            ((4,2),(3,3)),
            ((1,3),(0,4)),
            ((2,3),(1,4)),
            ((3,3),(2,4)),
            ((0,2),(0,3)),
            ((1,2),(1,3)),
            ((2,2),(2,3)),
            ((3,2),(3,3)),
            ((0,3),(0,4)),
            ((1,3),(1,4)),
            ((2,3),(2,4)),
            ((0,0),(1,0)),
            ((1,0),(2,0)),
            ((0,1),(1,1)),
            ((1,1),(2,1)),
            ((2,1),(3,1)),
            ((0,2),(1,2)),
            ((1,2),(2,2)),
            ((2,2),(3,2)),
            ((3,2),(4,2)),
            ((0,3),(1,3)),
            ((1,3),(2,3)),
            ((2,3),(3,3)),
            ((0,4),(1,4)),((1,4),(2,4)),((1,0),(0,0)),((2,0),(1,0)),((1,1),(0,1)),((2,1),(1,1)),((3,1),(2,1)),((1,2),(0,2)),((2,2),(1,2)),((3,2),(2,2)),((4,2),(3,2)),((1,3),(0,3)),((2,3),(1,3)),((3,3),(2,3)),((1,4),(0,4)),((2,4),(1,4)),((1,1),(0,0)),((2,1),(1,0)),((3,1),(2,0)),((1,2),(0,1)),((2,2),(1,1)),((3,2),(2,1)),((4,2),(3,1)),((0,1),(0,0)),((1,1),(1,0)),((2,1),(2,0)),((0,2),(0,1)),((1,2),(1,1)),((2,2),(2,1)),((3,2),(3,1)),((0,3),(0,2)),((1,3),(1,2)),((2,3),(2,2)),((3,3),(3,2)),((0,4),(0,3)),((1,4),(1,3)),((2,4),(2,3)),((0,3),(1,2)),((1,3),(2,2)),((2,3),(3,2)),((3,3),(4,2)),((0,4),(1,3)),((1,4),(2,3)),((2,4),(3,3))])


filterSlides :: Point -> [Slide] -> [Point]
filterSlides p slides = map  (snd)  (filter (\ x -> ((fst x) == p)) slides)

test_genSlides2 = TestCase (
    assertEqual
        "List of all possible slides size 2"
        (generateSlides [(0,0),(1,0),(0,1),(1,1),(2,1),(0,2),(1,2)] 2)
        [
            ((0,0),(0,1)),((0,0),(1,1)),((0,0),(1,0)),
            ((0,1),(0,0)),((0,1),(0,2)),((0,1),(1,1)),
            ((0,2),(1,2)),((0,2),(1,1)),((0,2),(0,1)),
            ((1,0),(2,1)),((1,0),(1,1)),((1,0),(0,0)),
            ((1,1),(0,2)),((1,1),(1,2)),((1,1),(2,1)),((1,1),(0,1)),((1,1),(0,0)),((1,1),(1,0)),
            ((1,2),(2,1)),((1,2),(1,1)),((1,2),(0,2)),
            ((2,1),(1,2)),((2,1),(1,1)),((2,1),(1,0))])

-- Game Over tests
gameOver_tests = TestList [test_samemove1, test_samemove2, test_fewW1, test_fewW2, test_fewW3]
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

-- Board Evaluator tests
boardEvaluator_tests = TestList []

mytree = Node 3 [W,W] [                  -- 10
            --max
            Node 3 [W,B] [               -- -25
                --min
                Node 3 [B,W,W,D] [],     -- -25
                Node 3 [W,B,B,D] [       -- 50
                    --max
                    Node 3 [B,B,B,B] [], -- 50
                    Node 3 [W,W,W,W] []  -- 5
                ]],
            Node 3 [W,W,W,D] [],         -- 10
            Node 3 [W,B,W,D] [],         -- 5
            Node 3 [W,W,D,D] []]         -- 5

myheuristic = myboardEvaluator W [] 3

myboardEvaluator :: Piece -> [Board] -> Int -> Board -> Bool -> Int
-- TODO
myboardEvaluator player history n board myTurn
    | board == [B,B,B,B] = 50
    | board == [B,W,W,D] = 25
    | board == [W,W,D,D] = 10
    | otherwise          = 5

