module Crusher where
-- CPSC 312 - Project 2
-- by Khurram Ali Jaffery

-- Main Components:
-- minimax algorithm
-- a board evaluator
-- state search
-- movement generators (and by extension, tree generator, new state generator)
-- crusher
-- custom data types (already done)

-- Piece is a data representation of possible pieces on a board
-- where D is an empty spot on the board
--		 W is a piece of the White player
--		 B is a piece of the Black player
--

data Piece = D | W | B deriving (Eq, Show)

--
-- Point is a tuple of 2 elements
-- representing a point on a grid system
-- where the first element represents the x coordinate
--       the second element represents the y coordinate
--

type Point = (Int, Int)

--
-- Tile is a tuple of 2 elements
-- representing what a point is occupied by
-- where the first element represents a piece
--       the second element represents a point
--

type Tile  = (Piece, Point)

--
-- Board is a list of Pieces, thus it is an internal representation
-- of the provided string representation of the board, it maintains
-- the same order as the string representation of the board
--

type Board = [Piece]

--
-- Grid is a list of Points, thus it is an internal representation
-- of the hexagonal grid system translated into a coordinate
-- system to easily maintain and make moves on the board
--

type Grid = [Point]

--
-- State is a list of Tile, thus it is an internal representation precisely
-- for the purposes of zipping the board and the grid together in order
-- to keep easier track of the effects on the pieces of making moves on grid
--

type State = [Tile]

--
-- Next is a data representation for storing and passing around information within
-- the tree generating function, allowing it to correctly generate new children
--
-- Next consists of 4 elements
-- where usedDepth is an integer representing the current depth level
--		 newBoard is the next board to add to the tree
-- 		 seenBoards is the updated history to avoid possible future trouble boards
-- 		 cplayer is the current player for whom the board was generated for
--

data Next a = Next {usedDepth :: Int, newBoard :: a, seenBoards :: [a], cplayer :: Piece}

--
-- Tree is a data representation for the search tree, it is an extention of
-- the rose tree widely used for implementing such unequally branched search trees
--
-- Tree consists of 3 elements
-- where depth is an integer representing the depth level of the node
-- 		 board is the game state at that node
-- 		 nextBoards are the child nodes of the current node
--

data Tree a = Node {depth :: Int, board :: a, nextBoards :: [Tree a]} deriving (Show)

--
-- BoardTree is the internal representation of the search tree of the given board
-- that is to be generatated for correctly implementing the minimax algorithm.
--

type BoardTree = Tree Board

--
-- Slide is a tuple of 2 elements
-- an internal representation of a slide
-- where the first element represents the point to move from
-- 		 the second element represents the adjacent point to move to
--

type Slide = (Point,Point)

--
-- Jump is a tuple of 3 elements
-- an internal representation of a leap
-- where the first element represents the point to move from
-- 		 the second element represents the adjacent point to move over
--		 the third element represents the point to move to
--

type Jump = (Point,Point,Point)

--
-- Move is a tuple of 2 elements
-- an internal representation of a move
-- where the first element represents the point to move from
-- 		 the second element represents the point to move to
--
-- Note: in essence it is the same as a slide however the idea
--		 is that a jump can be reduced to a move as in effect
--		 nothing happens the point moved over in a jump
--

type Move = (Point,Point)

--
-- Some test results to see what functions are producing
--
{-run = crusher ["W------------BB-BBB","----W--------BB-BBB","-W-----------BB-BBB"] 'W' 2 3
grid0 = generateGrid 3 2 4 []
slides0 = generateSlides grid0 3
jumps0 = generateLeaps grid0 3
board0 = sTrToBoard "WWW-WW-------BB-BBB"
newBoards0 = generateNewStates board0 [] grid0 slides0 jumps0 W
tree0 = generateTree board0 [] grid0 slides0 jumps0 W 4 3
heuristic0 = boardEvaluator W [] 3
-}
--
-- crusher
--
-- This function consumes a list of boards, a player, the depth of
-- search tree, the size of the provide boards, and produces the
-- next best board possible for the provided player, and accordingly
-- makes the move and returns new board consed onto the list of boards
--
-- Arguments:
-- -- (current:old): current represents the most recent board, old is
--                   the history of all boards already seen in game
-- -- p: 'W' or 'B' representing the player the program is
-- -- d: an Integer indicating depth of search tree
-- -- n: an Integer representing the dimensions of the board
--
-- Returns: a list of String with the new current board consed onto the front
--

crusher :: [String] -> Char -> Int -> Int -> [String]
crusher (current:old) player d size =
    [(boardToStr bestBoard)] ++ (current:old)
        where
            grid = (generateGrid size (size - 1) (2 * (size - 1)) [])
            bestBoard = stateSearch
                (sTrToBoard current)
                (map sTrToBoard old)
                grid
                (generateSlides grid size)
                (generateLeaps grid size)
                charToPiece
                d
                size
            charToPiece
                |  player == 'W' = W
                |  player == 'B' = B
                | otherwise = D
--
-- gameOver
--
-- This function consumes a board, a list of boards, and the dimension
-- of board and determines whether the given board is in a state where
-- the game has ended by checking if the board is present in the provided
-- list of boards or either the W or B pieces are less than dimension of board
--
-- Arguments:
-- -- board: a Board representing the most recent board
-- -- history: a list of Boards of representing all boards already seen
-- -- n: an Integer representing the dimensions of the board
--
-- Returns: True if the board is in a state where the game has ended, otherwise False
--

gameOver :: Board -> [Board] -> Int -> Bool
gameOver board history n
    | n == 0 = True
    | otherwise =
        board `elem` history ||
        (countPieces board W) < n  ||
        (countPieces board B) < n

countPieces :: Board -> Piece -> Int
countPieces board player =
    foldl (\acc x -> if x == player then acc + 1 else acc) 0 board

-- sTrToBoard
--
-- This function consumes a list of characters which can be either 'W' or 'B'
-- or '-' and converts them to a list of pieces, i.e W or B or D respectively
--
-- Arguments:
-- -- s: the String to convert into piece-wise representation
--
-- Note: This function would convert "WWW-WW-------BB-BBB" to
-- 	     [W,W,W,D,W,W,D,D,D,D,D,D,D,B,B,D,B,B,B]
--
-- Returns: the Board corresponding to the string
--

sTrToBoard :: String  -> Board
sTrToBoard s = map (\ x -> check x) s
    where
        check 'W' = W
        check 'B' = B
        check '-' = D

--
-- boardToStr
--
-- This function consumes a board which is a list of either W or B  or D and
-- converts them to a list of characters, i.e 'W' or 'B' or 'D' respectively
--
-- Arguments:
-- -- b: the Board to convert into char-wise representation
--
-- Note: This function would convert [W,W,W,D,W,W,D,D,D,D,D,D,D,B,B,D,B,B,B]
-- 	     to "WWW-WW-------BB-BBB"
--
-- Returns: the String corresponding to the board
--

boardToStr :: Board -> String
boardToStr b = map (\ x -> check x) b
    where
        check W = 'W'
        check B = 'B'
        check D = '-'

--
-- generateGrid
--
-- This function consumes three integers (described below) specifying how to
-- properly generate the grid and also a list as an accumulator; to generate a
-- regular hexagon of side length n, pass n (n- 1) (2 * (n - 1)) and []
--
-- Arguments:
-- -- n1: one more than max x-coordinate in the row, initialized always to n
-- -- n2: the number of rows away from the middle row of the grid
-- -- n3: the current y-coordinate i.e the current row number
-- -- acc: an accumulator that keeps track of accumulating rows of grid
--		   initialized to []
--
-- Note: This function on being passed 3 2 4 [] would produce
--		 [   (0,0),(1,0),(2,0)
--		     (0,1),(1,1),(2,1),(3,1)
--	         (0,2),(1,2),(2,2),(3,2),(4,2)
--		     (0,3),(1,3),(2,3),(3,3)
--		     (0,4),(1,4),(2,4)]
--
-- Returns: the corresponding Grid i.e the acc when n3 == -1
--

generateGrid :: Int -> Int -> Int -> Grid -> Grid
generateGrid n1 n2 n3 acc
    | n3 == -1      = acc
    | otherwise     = generateGrid nn1 (n2 - 1) (n3 - 1) (row ++ acc)
        where
            row = map (\ x -> (x,n3)) [0 .. (n1 - 1)]
            nn1 = if n2 > 0 then n1 + 1 else n1 - 1

--
-- generateSlides
--
-- This function consumes a grid and the size of the grid, accordingly
-- generates a list of all possible slides from any point on the grid to
-- any adjacent point on the grid
--
-- Arguments:
-- -- b: the Grid to generate slides for
-- -- n: an Integer representing the dimensions of the grid
--
-- Note: This function is only called at the initial setup of the game,
-- 		 it is a part of the internal representation of the game, this
--		 list of all possible slides is only generated once; and when
-- 		 generating next moves, the program decides which slides out of
--		 all these possible slides could a player actually make
--
-- Returns: the list of all Slides possible on the given grid
--
generateSlides :: Grid -> Int -> [Slide]
generateSlides b n = (removeDuplicateSlides (removeInvalidSlides (generateSlideHelper b n)))

-- generate unfiltered list of slides
generateSlideHelper :: Grid -> Int -> [Slide]
generateSlideHelper b n
    | n == 0 = []
    | null b = []
    | otherwise = (removeDuplicateSlides (removeInvalidSlides slides)) ++ (generateSlides (tail b) n)
                where sl = (slideLeft (head b) n)
                      sr = (slideRight (head b) n)
                      sul = (slideUpLeft (head b) n)
                      sdr = (slideDownRight (head b) n)
                      sur = (slideUpRight (head b)n )
                      sdl = (slideDownLeft (head b) n)
                      slides = [sl,sr,sul,sdr,sur,sdl]

removeInvalidSlides :: [Slide] -> [Slide]
removeInvalidSlides x
    | null x = []
    | (fst (head x)) == (snd (head x)) = removeInvalidSlides (tail x)
    | otherwise = head x : removeInvalidSlides (tail x)

removeDuplicateSlides :: [Slide] -> [Slide]
removeDuplicateSlides x
    | null x = []
    | head x `elem` tail x = removeDuplicateSlides (tail x)
    | otherwise = head x : removeDuplicateSlides (tail x)


slideLeft :: Point -> Int -> Slide
slideLeft x n
    | ((fst x) - 1) > -1 = (x, (((fst x) - 1), (snd x)))
    | otherwise = (x,x)

slideRight :: Point -> Int -> Slide
slideRight x n
    | (snd x) > (n - 1) = (slideRightBottom x n)
    | (snd x) <= (n - 1) = (slideRightTop x n)
    | otherwise = (x,x)

-- Helper for bottom half of hex for slideRight
slideRightBottom :: Point -> Int -> Slide
slideRightBottom x n
    | not((fst x) == n) && not(((snd x) == (n + 1)) && ((fst x) == (n - 1))) = (x, (((fst x) + 1), (snd x)))
    | otherwise = (x,x)

-- Helper for top half of hex for slideRight
slideRightTop :: Point -> Int -> Slide
slideRightTop x n
    | (((fst x) - (snd x)) /= 2) = (x, (((fst x) + 1), (snd x)))
    | otherwise = (x,x)

slideUpLeft :: Point -> Int -> Slide
slideUpLeft x n
    | (snd x) >= n = (bottomUpLeft x n)
    | (snd x) < n = (topUpLeft x n)
    | otherwise = (x,x)

-- Helper for bottom half of hex for slideUpLeft
bottomUpLeft :: Point -> Int -> Slide
bottomUpLeft x n
    | (((snd x) - 1) >= (n - 1)) = (x, ((fst x), ((snd x) - 1)))
    | otherwise = (x,x)

-- Helper for top half of hex for slideUpLeft
topUpLeft :: Point -> Int -> Slide
topUpLeft x n
    | not((fst x) == 0) && ((((snd x) - 1) >= 0)) = (x, (((fst x) - 1), ((snd x) - 1)))
    | otherwise = (x,x)

slideUpRight :: Point -> Int -> Slide
slideUpRight x n
    | (snd x) >= n = (bottomUpRight x n)
    | (snd x) < n = (topUpRight x n)
    | otherwise = (x,x)

-- Helper for bottom half of hex for slideUpRight
bottomUpRight :: Point -> Int -> Slide
bottomUpRight x n
    | (((fst x) + 1) <= (n + 1)) && (((snd x) - 1) >= (n - 2)) = (x, (((fst x) + 1), ((snd x) - 1)))
    | otherwise = (x,x)

-- Helper for top half of hex for slideUpRight
topUpRight :: Point -> Int -> Slide
topUpRight x n
    | (((fst x) - (snd x)) /= (n - 1)) && (((snd x) - 1) >= 0) = (x, ((fst x),((snd x) - 1)))
    | otherwise = (x,x)

slideDownRight :: Point -> Int -> Slide
slideDownRight x n
    | (snd x) >= n - 1 = (bottomDownRight x n)
    | (snd x) < n - 1 = (topDownRight x n)
    | otherwise = (x,x)

-- Helper for bottom half of hex for slideDownRight
bottomDownRight :: Point -> Int -> Slide
bottomDownRight x n
    | ((((snd x) + 1) <= (n + 1)) && not(((fst x) == n) && ((snd x) == n)) && ((fst x) /= (n + 1)))= (x, ((fst x), ((snd x) + 1)))
    | otherwise = (x,x)

-- Helper for top half of hex for slideDownRight
topDownRight :: Point -> Int -> Slide
topDownRight x n
    | (((fst x) + 1) <= (n + 1)) = (x, (((fst x) + 1), ((snd x) + 1)))
    | otherwise = (x,x)

slideDownLeft :: Point -> Int -> Slide
slideDownLeft x n
    | (snd x) >= (n - 1) = (bottomDownLeft x n)
    | (snd x) < (n - 1) = (topDownLeft x n)
    | otherwise = (x,x)

-- Helper for bottom half of hex for slideDownLeft
bottomDownLeft :: Point -> Int -> Slide
bottomDownLeft x n
    | ((fst x) /= 0) && (((snd x) + 1) <= (n + 1)) = (x, (((fst x) - 1), ((snd x) + 1)))
    | otherwise = (x,x)

-- Helper for top half of hex for slideDownLeft
topDownLeft :: Point -> Int -> Slide
topDownLeft x n
    | (((snd x) + 1) <= n) =  (x, ((fst x), ((snd x) + 1)))
    | otherwise = (x,x)


--
-- generateLeaps
--
-- This function consumes a grid and the size of the grid, accordingly
-- generates a list of all possible leaps from any point on the grid over
-- any adjacent point on the grid to any point next to the adjacent point
-- such that it is movement in the same direction
--
-- Arguments:
-- -- b: the Grid to generate leaps for
-- -- n: an Integer representing the dimensions of the grid
--
-- Note: This function is only called at the initial setup of the game,
-- 		 it is a part of the internal representation of the game, this
--		 list of all possible leaps is only generated once; and when
-- 		 generating next moves, the program decides which leaps out of
--		 all these possible leaps could a player actually make
--
-- Returns: the list of all Jumps possible on the given grid
--

generateLeaps :: Grid -> Int -> [Jump]
generateLeaps b n = (removeInvalidJumps (generateLeapsHelper b n))

-- generate unfiltered list of Jumps
generateLeapsHelper :: Grid -> Int -> [Jump]
generateLeapsHelper b n
    | n == 0 = []
    | null b = []
    | otherwise = leaps ++ (generateLeaps (tail b) n)
                where ll = (leapLeft (head b) n)
                      lr = (leapRight (head b) n)
                      ldul = (leapDiagonalUpLeft (head b) n)
                      ldur = (leapDiagonalUpRight (head b) n)
                      lddl = (leapDiagonalDownLeft (head b) n)
                      lddr = (leapDiagonalDownRight (head b) n)
                      leaps = [ll,lr,ldul,ldur,lddl,lddr]


removeInvalidJumps :: [Jump] -> [Jump]
removeInvalidJumps x
    | null x = []
    | ((getFirst (head x)) == (getThird (head x))) || ((getFirst (head x)) == (getSecond (head x))) || ((getSecond (head x)) == (getThird (head x))) = removeInvalidJumps (tail x)
    | otherwise = (head x) : removeInvalidJumps (tail x)

-- Helpers to grab value from a triple
getThird :: Jump -> Point
getThird (_, _, x) = x

getSecond :: Jump -> Point
getSecond (_, x, _) = x

getFirst :: Jump -> Point
getFirst (x, _, _) = x

-- Leaps
leapLeft :: Point -> Int -> Jump
leapLeft x n = (right, mid, left)
     where  right = (fst (slideLeft x n))
            mid = (snd (slideLeft x n))
            left = (snd (slideLeft mid n))

leapRight :: Point -> Int -> Jump
leapRight x n = (left, mid, right)
    where   left = (fst (slideRight x n))
            mid = (snd (slideRight x n))
            right = (snd (slideRight mid n))

leapDiagonalUpLeft :: Point -> Int -> Jump
leapDiagonalUpLeft x n = (bottom, mid, topleft)
    where   bottom = (fst (slideUpLeft x n))
            mid = (snd (slideUpLeft x n))
            topleft = (snd (slideUpLeft mid n))

leapDiagonalUpRight :: Point -> Int -> Jump
leapDiagonalUpRight x n = (bottom, mid, topright)
    where   bottom = (fst (slideUpRight x n))
            mid = (snd (slideUpRight x n))
            topright = (snd (slideUpRight mid n))

leapDiagonalDownLeft :: Point -> Int -> Jump
leapDiagonalDownLeft x n = (top, mid , bottomleft)
    where   top = (fst (slideDownLeft x n))
            mid = (snd (slideDownLeft x n))
            bottomleft = (snd (slideDownLeft mid n))

leapDiagonalDownRight :: Point -> Int -> Jump
leapDiagonalDownRight x n = (top, mid , bottomright)
    where   top = (fst (slideDownRight x n))
            mid = (snd (slideDownRight x n))
            bottomright = (snd (slideDownRight mid n))

--
-- stateSearch
--
-- This function consumes the arguments described below, based on the internal
-- representation of the game, if there is no point in playing the game as the
-- current board is in a state where the game has ended then just return the
-- board, else generate a search tree till the specified depth and apply
-- minimax to it by using the appropriately generated heuristic
--
-- Arguments:
-- -- board: a Board representing the most recent board
-- -- history: a list of Boards of representing all boards already seen
-- -- grid: the Grid representing the coordinate-grid the game being played
-- -- slides: the list of all Slides possible for the given grid
-- -- jumps: the list of all Jumps possible for the given grid
-- -- player: W or B representing the player the program is
-- -- depth: an Integer indicating depth of search tree
-- -- num: an Integer representing the dimensions of the board
--
-- Returns: the current board if game is over,
--          otherwise produces the next best board
--

stateSearch :: Board -> [Board] -> Grid -> [Slide] -> [Jump] -> Piece -> Int -> Int -> Board
stateSearch board history grid slides jumps player depth size
    | (gameOver board history size) = board
    | otherwise = minimax tree heuristic
        where
            tree = generateTree board history grid slides jumps player depth size
            heuristic = boardEvaluator player history size
--
-- generateTree
--
-- This function consumes the arguments described below, and builds a search
-- tree till specified depth from scratch by using the current board and
-- generating all the next states recursively; however it doesn't generate
-- children of those states which are in a state where the game has ended.
--
-- Arguments:
-- -- board: a Board representing the most recent board
-- -- history: a list of Boards of representing all boards already seen
-- -- grid: the Grid representing the coordinate-grid the game being played
-- -- slides: the list of all Slides possible for the given grid
-- -- jumps: the list of all Jumps possible for the given grid
-- -- player: W or B representing the player the program is
-- -- depth: an Integer indicating depth of search tree
-- -- n: an Integer representing the dimensions of the board
--
-- Returns: the corresponding BoardTree generated till specified depth
--

generateTree :: Board -> [Board] -> Grid -> [Slide] -> [Jump] -> Piece -> Int -> Int -> BoardTree
generateTree board history grid slides jumps player depth n
    | depth == 0    = (Node depth board [])
    | otherwise     = (Node depth board children)
            where
                nextStates = generateNewStates board history grid slides jumps player
                children = generateTreeHelper nextStates history grid slides jumps player (depth - 1) n


generateTreeHelper :: [Board] -> [Board] -> Grid -> [Slide] -> [Jump] -> Piece -> Int -> Int -> [Tree Board]
generateTreeHelper brds history grid slides jumps player depth n
    | null brds     = []
    | otherwise     = (newHead:newTail)
        where
            newHead = generateTree (head brds) history grid slides jumps otherPlayer depth n
            newTail = generateTreeHelper (tail brds) history grid slides jumps player depth n
            otherPlayer = if player == W then B else W

-- generateNewStates
--
-- This function consumes the arguments described below, it first generates a
-- list of valid moves, applies those moves to the current board to generate
-- a list of next boards, and then checks whether or not that move would
-- have been possible by filtering out those boards already seen before
--
-- Arguments:
-- -- board: a Board representing the most recent board
-- -- history: a list of Boards of representing all boards already seen
-- -- grid: the Grid representing the coordinate-grid the game being played
-- -- slides: the list of all Slides possible for the given grid
-- -- jumps: the list of all Jumps possible for the given grid
-- -- player: W or B representing the player the program is
--
-- Returns: the list of next boards
--

generateNewStates :: Board -> [Board] -> Grid -> [Slide] -> [Jump] -> Piece -> [Board]
generateNewStates board history grid slides jumps player = validBoards
    where state = zip board grid
          validMoves = (moveGenerator state slides jumps player)
          newBoards = (generateNewBoards validMoves state player)
          validBoards = findBoardsAlreadySeen newBoards history

findBoardsAlreadySeen :: [Board] -> [Board] -> [Board]
findBoardsAlreadySeen boards history
    | null history = boards
    | null boards = []
    | (head boards) `elem` history = findBoardsAlreadySeen (tail boards) history
    | otherwise = (head boards) : findBoardsAlreadySeen (tail boards) history

generateNewBoards :: [Move] -> State -> Piece -> [Board]
generateNewBoards moves state player
    | null moves = []
    | otherwise = (stateToBoard newState) : generateNewBoards (tail moves) state player
        where move = (head moves)
              newState = (findMoveAndReplacePointInState move state player)

stateToBoard :: State -> Board
stateToBoard s = map fst s

findMoveAndReplacePointInState :: Move -> State -> Piece -> State
findMoveAndReplacePointInState move state player
    | null state = []
    | (fst move) == (snd (head state)) = (D, (snd move)) : findMoveAndReplacePointInState move (tail state) player
    | (snd move) == (snd (head state)) = (player, (snd move)) : findMoveAndReplacePointInState move (tail state) player
    | otherwise = (head state) : findMoveAndReplacePointInState move (tail state) player

--
-- moveGenerator
--
-- This function consumes a state, a list of possible jumps,
-- a list of possible slides and a player from whose perspective
-- to generate moves, to check which of these jumps and slides
-- the player could actually make, and produces a list of valid moves
--
-- Arguments:
-- -- state: a State representing the most recent state
-- -- slides: the list of all Slides possible for the given grid
-- -- jumps: the list of all Jumps possible for the given grid
-- -- player: W or B representing the player the program is
--
-- Note: This is the only instance where the program makes use of the
--		 type State, for our purposes it is zipping the board and the
--		 grid together for making it easier to make moves.
--
-- Note:
-- -- oP is opponentsPieces
-- -- pP is playersPieces
-- -- vS is validSlides
-- -- vJ is validJumps
--
-- Returns: the list of all valid moves that the player could make
--

moveGenerator :: State -> [Slide] -> [Jump] -> Piece -> [Move]
moveGenerator state slides jumps player =
    (validJumps jumps state player) ++ (validSlides slides state player)

-- Generate a list of all valid Jumps from state of the board
validJumps :: [Jump] -> [Tile] -> Piece -> [Move]
validJumps x t player
    | null x = []
    | ((getFirst (head x)) `elem` startingPoint) &&
      ((getSecond (head x)) `elem` middlePoint) &&
      ((getThird (head x)) `elem` endingPoint) =
        ((getFirst (head x)), (getThird (head x))) : validJumps (tail x) t player
    | otherwise = validJumps (tail x) t player
        where startingPoint = filterJumpStartingTiles (head x) t player
              middlePoint = filterJumpMiddleTiles (head x) t player
              endingPoint = filterJumpEndingTiles (head x) t player

filterJumpStartingTiles :: Jump -> [Tile] -> Piece -> [Point]
filterJumpStartingTiles x t player
    | null t = []
    | ((getFirst x) == (snd (head t))) && ((fst (head t)) == player) = (snd (head t)) : filterJumpStartingTiles x (tail t) player
    | otherwise = filterJumpStartingTiles x (tail t) player

filterJumpMiddleTiles :: Jump -> [Tile] -> Piece -> [Point]
filterJumpMiddleTiles x t player
    | null t = []
    | ((getSecond x) == (snd (head t))) && ((fst (head t)) == player) = (snd (head t)) : filterJumpMiddleTiles x (tail t) player
    | otherwise = filterJumpMiddleTiles x (tail t) player

filterJumpEndingTiles :: Jump -> [Tile] -> Piece -> [Point]
filterJumpEndingTiles x t player
    | null t = []
    | ((getThird x) == (snd (head t))) && ((fst (head t)) /= player) = (snd (head t)) : filterJumpEndingTiles x (tail t) player
    | otherwise = filterJumpEndingTiles x (tail t) player

-- Generate a list of all valid slides from the state of the board
validSlides :: [Slide] -> [Tile] -> Piece -> [Move]
validSlides x t player
    | null x = []
    | ((fst (head x)) `elem` startingPoint) &&
      ((snd (head x)) `elem` endingPoint) =
        (head x) : validSlides (tail x) t player
    | otherwise = validSlides (tail x) t player
        where startingPoint = filterSlideStartingTiles (head x) t player
              endingPoint = filterSlideEndingTiles (head x) t player

-- All the points that have a player piece
filterSlideStartingTiles :: Slide -> [Tile] -> Piece -> [Point]
filterSlideStartingTiles x t player
    | null t = []
    | ((fst x) == (snd (head t))) && ((fst (head t)) == player) = (snd (head t)) : filterSlideStartingTiles x (tail t) player
    | otherwise = filterSlideStartingTiles x (tail t) player

-- All the points that have a empty spot piece
filterSlideEndingTiles :: Slide -> [Tile] -> Piece -> [Point]
filterSlideEndingTiles x t p
    | null t = []
    | ((snd x) == (snd (head t))) && (((fst (head t))) == D) = (snd (head t)) : filterSlideEndingTiles x (tail t) p
    | otherwise = filterSlideEndingTiles x (tail t) p


--
-- boardEvaluator
--
-- This function consumes a board and performs a static board evaluation, by
-- taking into account whose perspective the program is playing from, the list
-- of boards already seen, the size of the board, and whether or not it is the
-- program's turn or not; to generate quantitative measures of the board, and
-- accordingly produce a goodness value of the given board
--
-- Arguments:
-- -- player: W or B representing the player the program is
-- -- history: a list of Boards of representing all boards already seen
-- -- n: an Integer representing the dimensions of the board
-- -- board: a Board representing the most recent board
-- -- myTurn: a Boolean indicating whether it is the program's turn or the opponents.
--
-- Returns: the goodness value of the provided board
--

-- Heuristic used:
-- If won the board, 100
-- If lost the board, -100
-- if can crunch opponent, n*25
-- if can be crunshed by opponent, n*-25
-- # leaps, n*10
-- # slides, n*1

boardEvaluator :: Piece -> [Board] -> Int -> Board -> Bool -> Int
-- TODO
boardEvaluator player history n board myTurn
    | won player board myTurn   = 100
    | lost player board myTurn  = -100
    | otherwise =
        (crunchCount * 25) +
        (crunchedCount * (-25)) +
        (leapCount * 10) +
        (slideCount * 1)
            where
                crunchCount = 1
                crunchedCount = 0
                leapCount = 2
                slideCount = 5

won :: Piece -> Board -> Bool -> Bool
won player board myTurn = False

lost :: Piece -> Board -> Bool -> Bool
lost player board myTurn = not False
--
-- minimax
--
-- This function implements the minimax algorithm, it consumes a search tree,
-- and an appropriate heuristic to apply to the tree, by applying minimax it
-- produces the next best board that the program should make a move to
--
-- Arguments:
-- -- (Node _ b children): a BoardTree to apply minimax algorithm on
-- -- heuristic: a partially evaluated boardEvaluator representing the
--				 appropriate heuristic to apply based on the size of the board,
--				 who the program is playing as, and all the boards already seen
--
-- Returns: the next best board
--

minimax :: BoardTree -> (Board -> Bool -> Int) -> Board
minimax (Node _ b children) heuristic = [W,W,W,D,W,W,D,D,D,D,D,D,D,B,B,D,B,B,B]

-- minimize opponent's move and maximize my moves
-- calculate the goodness value at each leaf node

-- minimax'
--
-- This function is a helper to the actual minimax function, it consumes
-- a search tree, an appropriate heuristic to apply to the leaf nodes of
-- the tree, and based on whether it would have been the maximizing
-- player's turn, it accordingly propagates the values upwards until
-- it reaches the top to the base node, and produces that value.
--
-- Arguments:
-- -- (Node _ b []): a BoardTree
-- -- (Node _ b children): a BoardTree
-- -- heuristic: a partially evaluated boardEvaluator representing the
--				 appropriate heuristic to apply based on the size of the board,
--				 who the program is playing as, and all the boards already seen
-- -- maxPlayer: a Boolean indicating whether the function should be maximizing
-- 				 or miniziming the goodness values of its children
--
-- Returns: the minimax value at the top of the tree
--

minimax' :: BoardTree -> (Board -> Bool -> Int) -> Bool -> Int
minimax' boardTree heuristic maxPlayer = 4


play :: [String] -> Char -> Int -> Int -> IO ()
play history@(current:old) player depth n
  | gameOver (sTrToBoard current) (map sTrToBoard old) n = putStrLn "Game over."
  | otherwise = do
       let history'@(new:_) = crusher history player depth n
       putStrLn $ player:" played: " ++ new
       play history' (if player == 'W' then 'B' else 'W') depth n

myminimax :: BoardTree -> [Board]
myminimax (Node _ board children)
    | null children = [board]
    | otherwise  = myminimaxhelper children

myminimaxhelper :: [Tree Board] -> [Board]
myminimaxhelper (node:rest)
    | null rest = myminimax node
    | otherwise = (myminimax node) ++ (myminimaxhelper rest)


mytree = Node 3 [W,W] [
            Node 3 [W,B] [
                Node 3 [B,W,W,D] [],
                Node 3 [W,B,B,D] [
                    Node 3 [B,B,B,B] [],
                    Node 3 [W,W,W,W] []
                ]],
            Node 3 [W,W,W,D] [],
            Node 3 [W,W,W,D] [],
            Node 3 [W,W,D,D] []]
