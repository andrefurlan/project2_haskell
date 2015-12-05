module Crusher where


-- Andre Furlan Bueno - o3d9 - 45222130
-- Rennie Haylock - p1a9 - 57117137
-- Andrew Li - h2e9 - 11948130

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
run = crusher ["W------------BB-BBB","----W--------BB-BBB","-W-----------BB-BBB"] 'W' 2 3
grid0 = generateGrid 3 2 4 []
slides0 = generateSlides grid0 3
jumps0 = generateLeaps grid0 3
board0 = sTrToBoard "WWW-WW-------BB-BBB"
newBoards0 = generateNewStates board0 [] grid0 slides0 jumps0 W
tree0 = generateTree board0 [] grid0 slides0 jumps0 W 4 3
heuristic0 = boardEvaluator W grid0 3

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
    -- gets the best board from stateSearch
    let bestBoard = stateSearch board history grid slides leaps charToPiece d size
    -- append the new best board to the head of the history
    in (boardToStr bestBoard):(current:old)
        where
            grid = generateGrid size (size - 1) (2 * (size - 1)) []
            slides = generateSlides grid size
            leaps = generateLeaps grid size
            board = sTrToBoard current
            history = map sTrToBoard old
            charToPiece
                | player == 'W' = W
                | player == 'B' = B
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
        -- checks if the board has been played before
        board `elem` history ||
        -- checks if neither players have less than n pieces
        (countPieces board W) < n  ||
        (countPieces board B) < n

-- countPieces

-- Counts the number of a pieces a player has in the board

-- Arguments:
-- -- board: the board to use in the count
-- -- player: the piece to count (W/B)

-- Returns: the number of pieces in board

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
--		   (0,1),(1,1),(2,1),(3,1)
--	    (0,2),(1,2),(2,2),(3,2),(4,2)
--		   (0,3),(1,3),(2,3),(3,3)
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
-- generates all possible slides and filter them so only possible ones are
-- returned. This would be much easier on the new grid used on generateLeaps
generateSlides grid n = filter (\ x -> (elem (snd x) grid))
     -- move south-east top (n-1) rows
    ([(x,(((fst x) + 0),((snd x) + 1))) | x <- grid, (snd x) <  (n-1)]++
     -- move south-west top (n-1) rows
     [(x,(((fst x) + 1),((snd x) + 1))) | x <- grid, (snd x) <  (n-1)]++
     -- move south-east bottom n rows
     [(x,(((fst x) - 1),((snd x) + 1))) | x <- grid, (snd x) >= (n-1)]++
     -- move south-west bottom n rows
     [(x,(((fst x) + 0),((snd x) + 1))) | x <- grid, (snd x) >= (n-1)]++
     -- move east
     [(x,(((fst x) + 1),((snd x) + 0))) | x <- grid]++
     -- move west
     [(x,(((fst x) - 1),((snd x) + 0))) | x <- grid]++
     -- move north-west top n rows
     [(x,(((fst x) - 1),((snd x) - 1))) | x <- grid, (snd x) <= (n-1)]++
     -- move north-east top n rows
     [(x,(((fst x) + 0),((snd x) - 1))) | x <- grid, (snd x) <= (n-1)]++
     -- move north-west bottom (n-1) rows
     [(x,(((fst x) + 0),((snd x) - 1))) | x <- grid, (snd x) >  (n-1)]++
     -- move north-east bottom (n-1) rows
     [(x,(((fst x) + 1),((snd x) - 1))) | x <- grid, (snd x) >  (n-1)])

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

-- TODO: This function turned out to be hard to do in the original
-- grid system, so I converted to a new grid system (see convertPoint)
-- If we have time, it would be good to create this new grid in
-- generateGrid, so everything would be smooth. But I don't know the
-- implications to change the other functions, so I leave it here
-- for now.

generateLeaps :: Grid -> Int -> [Jump]
-- creates a grid with coordinate system that is actually
-- sensible, list all possible jumps using that grid, filter
-- the jumps that go out of the grid, and revert the jumps
-- to the original grid coordinate system.
generateLeaps grid n = revertJumpsToOldGrid $ filterJumps $ listAllJumps sensibleGrid
    where
        -- only keep jumps that land inside the grid
        filterJumps jumps = filter (\ x -> (elem (trd' x) sensibleGrid)) jumps
        -- get a grid with an easy to deal coordinate system
        sensibleGrid = map (\x -> (convertPoint (+) x n)) grid
        -- reverts each Point in the jump list to old grid system
        revertJumpsToOldGrid jumps = map (\x -> (revertFun x)) jumps
        revertFun x = (
            (convertPoint (-) (fst' x) n),
            (convertPoint (-) (snd' x) n),
            (convertPoint (-) (trd' x) n))

-- listAllJumps
--
-- This function takes a grid and generates a list of all the
-- six directions a jump can go to from all points in the grid

-- Arguments:
-- -- grid: Grid

-- Returns:
-- -- the list of all jumps (even impossible ones)

listAllJumps :: Grid -> [Jump]
listAllJumps grid =
    -- jump south-west
    [(x, ( ((fst x) + 0), ((snd x) + 1) ), ( ((fst x) + 0), ((snd x) + 2) ) ) | x <- grid]++
    -- jump south-east
    [(x, ( ((fst x) + 1), ((snd x) + 1) ), ( ((fst x) + 2), ((snd x) + 2) ) ) | x <- grid]++
    -- jump east
    [(x, ( ((fst x) + 1), ((snd x) + 0) ), ( ((fst x) + 2), ((snd x) + 0) ) ) | x <- grid]++
    -- jump west
    [(x, ( ((fst x) - 1), ((snd x) + 0) ), ( ((fst x) - 2), ((snd x) + 0) ) ) | x <- grid]++
    -- jump north-west
    [(x, ( ((fst x) - 1), ((snd x) - 1) ), ( ((fst x) - 2), ((snd x) - 2) ) ) | x <- grid]++
    -- jump north-east
    [(x, ( ((fst x) + 0), ((snd x) - 1) ), ( ((fst x) + 0), ((snd x) - 2) ) ) | x <- grid]

-- convertPoint

-- This function applies a function to the (n-1) rows of the grid
-- to adjust the point so actual position based calculations are
-- easy to do.

-- Arguments:
-- -- f: a function as (+) or (-)
-- -- p: a point
-- -- n: size of grid

-- Note:
-- In a normal grid, the numbers on the right will be applied to
-- to point belonging to the respective row, producing the points in
-- the grid in a way that the direction can be checked by easily checking
-- if the column number follows the pattern. So instead of checking in a jump
-- (4-2) (3,3) (2,4), we can check (4,2) (4,3) (4,4).

--          Original grid points      Difference
--		 [   (0-0),(1,0),(2-0)            0
--		  (0,1),(1,1),(2,1),(3,1)         0
--	   (0-2),(1,2),(2-2),(3,2),(4-2)      0
--		  (0,3),(1,3),(2,3),(3,3)         1
--		     (0-4),(1,4),(2-4)]           2

--            New grid points
--		 [   (0-0),(1,0),(2-0)
--		  (0,1),(1,1),(2,1),(3,1)
--	   (0-2),(1,2),(2-2),(3,2),(4-2)
--	      (1,3),(2,3),(3,3),(4,3)
--		     (2-4),(3,4),(4-4)]



convertPoint :: (Int -> Int -> Int) -> Point -> Int -> Point
convertPoint f p n = ( f (fst p) (([0 | x <- [1..n]]++[1..(n-1)])!!(snd p)), snd p )

-- Helpers to grab value from a triple
trd' :: Jump -> Point
trd' (_,_,x) = x

snd' :: Jump -> Point
snd' (_,x,_) = x

fst' :: Jump -> Point
fst' (x,_,_) = x


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
    -- return the same board if the game has ended
    | (gameOver board history size) = board
    -- call minimax the with move tree from the current board
    -- which will returnthe best board given a specific heuristic
    | otherwise                     = minimax tree heuristic
        where
            tree = generateTree board history grid slides jumps player depth size
            heuristic = boardEvaluator player grid size

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
    -- base case, depth of zero, just return a tree with the root (could be a leaf)
    | depth == 0                = (Node depth board [])
    -- no new children for game over boards
    | gameOver board history n  = (Node depth board [])
    -- return the tree with children, and recursively build the tree until leaves.
    -- this takes care of the first level of the tree, the rest is build using generateTreeHelper
    | otherwise                 = (Node depth board children)
            where
                -- list all possible states from a board
                nextStates = generateNewStates board history grid slides jumps player
                -- build a list of tree nodes with max depth n-1 as the children (reduction step here)
                children = generateTreeHelper nextStates history grid slides jumps player (depth - 1) n

--
-- generateTreeHelper
--
-- This function consumes the arguments below and creates the tree nodes
-- for the children of a given board in generateTree function. It recursively
-- construct the tree for each child node.
--
-- Arguments:
-- -- brds: a list of Board representing the children of a board
-- -- history: a list of Boards of representing all boards already seen
-- -- grid: the Grid representing the coordinate-grid the game being played
-- -- slides: the list of all Slides possible for the given grid
-- -- jumps: the list of all Jumps possible for the given grid
-- -- player: W or B representing the player the program is
-- -- depth: an Integer indicating depth of search tree
-- -- n: an Integer representing the dimensions of the board
--
-- Returns: the list of Tree Board representing the children of list board in GenerateTree
--

generateTreeHelper :: [Board] -> [Board] -> Grid -> [Slide] -> [Jump] -> Piece -> Int -> Int -> [Tree Board]
generateTreeHelper brds history grid slides jumps player depth n
    -- if no more moves can be made, then just return a empty list
    | null brds     = []
    -- build a list of trees recursively
    | otherwise     = newHead:newTail
        where
            -- for each element of the tree, generate a node with children.
            -- important to note that for each depth the player alternates so minimax works
            -- correctly
            newHead = generateTree (head brds) history grid slides jumps otherPlayer depth n
            newTail = generateTreeHelper (tail brds) history grid slides jumps player depth n
            otherPlayer = swapPlayer player

-- swapPlayer

-- If given W, returns B. If given B returns W.

swapPlayer :: Piece -> Piece
swapPlayer player = if player == W then B else W


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

-- findBoardsAlreadySeen
--
-- This functions filters takes a list of newly generated boards from generateNewBoards
-- and removes it if they exist in the history
--
-- Arguments:
-- -- boards: list of newly created boards based on the current state
-- -- history: a list of Boards of representing all boards already seen
--
-- Returns: A list of Boards that are not in history
--

findBoardsAlreadySeen :: [Board] -> [Board] -> [Board]
findBoardsAlreadySeen boards history
    | null history = boards
    | null boards = []
    | (head boards) `elem` history = findBoardsAlreadySeen (tail boards) history
    | otherwise = (head boards) : findBoardsAlreadySeen (tail boards) history

--
-- generateNewBoards
-- This function takes a list of moves and the state of the board
-- and generates all possible boards with the given move of the player
-- that is up using the findMoveAndReplacePointInState function
--
-- Arguments:
-- -- moves: List of moves for the given state
-- -- state: The current state of the board
-- -- player: W or B representing the player the program is
--
-- Returns: A list of boards created with the moves of the player
--

generateNewBoards :: [Move] -> State -> Piece -> [Board]
generateNewBoards moves state player
    | null moves = []
    | otherwise = (stateToBoard newState) : generateNewBoards (tail moves) state player
        where move = (head moves)
              newState = (findMoveAndReplacePointInState move state player)

-- stateToBoard
-- This function consumes a state and returns the given state
-- in a representation of a board
--
-- Arguments:
-- -- s: state to be converted to board
--
-- Returns: a board representing the given state

stateToBoard :: State -> Board
stateToBoard s = map fst s

-- findMoveAndReplacePointInState
--
-- This function takes a move and replaces the first point of the move to
-- be an empty spot on the board, then it take the second point of the move and
-- replaces it with the newly assigned piece to the point thus creating a tile
-- which will then be cons together to form a State. Otherwise, leave the Tile
-- alone and check the next tile against the move
--
-- Arguments:
-- -- move: The move the game is trying to make
-- -- state: The state of the current game
-- -- player: W or B representing the player the program is
--
-- Returns: A new state with the move executed
--

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

--
-- validJumps
--
-- This functions takes in a list of jumps and a list of tiles (state)
-- and the player that is up to generate a list of all valid Jumps
-- from the state of the board
--
-- Arguments:
-- -- jumps: the list of all Jumps possible for the given grid
-- -- tiles:  a State representing the most recent state
-- -- player: W or B representing the player the program is
--
-- Returns: List of validJumps in the form of list of valid Move
--

validJumps :: [Jump] -> [Tile] -> Piece -> [Move]
validJumps jumps tiles player
    | null jumps = []
    | ((fst' (head jumps)) `elem` startingPoint) &&
      ((snd' (head jumps)) `elem` middlePoint) &&
      ((trd' (head jumps)) `elem` endingPoint) =
        ((fst' (head jumps)), (trd' (head jumps))) : validJumps (tail jumps) tiles player
    | otherwise = validJumps (tail jumps) tiles player
        where startingPoint = filterJumpStartingTiles (head jumps) tiles player
              middlePoint = filterJumpMiddleTiles (head jumps) tiles player
              endingPoint = filterJumpEndingTiles (head jumps) tiles player

--
-- filterJumpStartingTiles
--
-- This functions filters out all the valid starting points for a given jump
-- in the current state of the board represtented as a list of tiles
--
-- Arguments:
-- -- jmp: the jump that is currently compared to in the current state
-- -- t:  a State representing the most recent state
-- -- player: W or B representing the player the program is
--
-- Returns: A list of all valid starting points for the jump
--

filterJumpStartingTiles :: Jump -> [Tile] -> Piece -> [Point]
filterJumpStartingTiles jmp t player
    | null t = []
    | ((fst' jmp) == (snd (head t))) && ((fst (head t)) == player) = (snd (head t)) : filterJumpStartingTiles jmp (tail t) player
    | otherwise = filterJumpStartingTiles jmp (tail t) player

--
-- filterJumpMiddleTiles
--
-- This functions filters out all the valid middle points for a given jump
-- in the current state of the board represtented as a list of tiles
--
-- Arguments:
-- -- jmp: the jump that is currently compared to in the current state
-- -- t:  a State representing the most recent state
-- -- player: W or B representing the player the program is
--
-- Returns: A list of all valid middle points for the jump
--

filterJumpMiddleTiles :: Jump -> [Tile] -> Piece -> [Point]
filterJumpMiddleTiles jmp t player
    | null t = []
    | ((snd' jmp) == (snd (head t))) && ((fst (head t)) == player) = (snd (head t)) : filterJumpMiddleTiles jmp (tail t) player
    | otherwise = filterJumpMiddleTiles jmp (tail t) player

--
-- filterJumpEndingTiles
--
-- This functions filters out all the valid ending points for a given jump
-- in the current state of the board represtented as a list of tiles
--
-- Arguments:
-- -- jmp: the jump that is currently compared to in the current state
-- -- t:  a State representing the most recent state
-- -- player: W or B representing the player the program is
--
-- Returns: A list of all valid ending points for the jump
--

filterJumpEndingTiles :: Jump -> [Tile] -> Piece -> [Point]
filterJumpEndingTiles jmp t player
    | null t = []
    | ((trd' jmp) == (snd (head t))) && ((fst (head t)) /= player) = (snd (head t)) : filterJumpEndingTiles jmp (tail t) player
    | otherwise = filterJumpEndingTiles jmp (tail t) player

--
-- validSlides
--
-- This functions takes in a list of slides and a list of tiles (state)
-- and the player that is up to generate a list of all valid slides
-- from the state of the board
--
-- Arguments:
-- -- slides: the list of all Jumps possible for the given grid
-- -- t:  a State representing the most recent state
-- -- player: W or B representing the player the program is
--
-- Returns: List of validSlides in the form of list of valid Move
--

validSlides :: [Slide] -> [Tile] -> Piece -> [Move]
validSlides slides t player
    | null slides = []
    | ((fst (head slides)) `elem` startingPoint) &&
      ((snd (head slides)) `elem` endingPoint) =
        (head slides) : validSlides (tail slides) t player
    | otherwise = validSlides (tail slides) t player
        where startingPoint = filterSlideStartingTiles (head slides) t player
              endingPoint = filterSlideEndingTiles (head slides) t player

--
-- filterSlideStartingTiles
--
-- This functions filters out all the valid starting points for a given slide
-- in the current state of the board represtented as a list of tiles, which is
-- all the points that have a player piece
--
-- Arguments:
-- -- sld: the slide that is currently compared to in the current state
-- -- t:  a State representing the most recent state
-- -- player: W or B representing the player the program is
--
-- Returns: A list of all valid starting points for the slide
--

filterSlideStartingTiles :: Slide -> [Tile] -> Piece -> [Point]
filterSlideStartingTiles sld t player
    | null t = []
    | ((fst sld) == (snd (head t))) && ((fst (head t)) == player) = (snd (head t)) : filterSlideStartingTiles sld (tail t) player
    | otherwise = filterSlideStartingTiles sld (tail t) player

--
-- filterSlideEndingTiles
--
-- This functions filters out all the valid ending points for a given slide
-- in the current state of the board represtented as a list of tiles, which is
-- all the points that have a empty spot piece
--
-- Arguments:
-- -- sld: the slide that is currently compared to in the current state
-- -- t:  a State representing the most recent state
-- -- player: W or B representing the player the program is
--
-- Returns: A list of all valid ending points for the slide
--

filterSlideEndingTiles :: Slide -> [Tile] -> Piece -> [Point]
filterSlideEndingTiles sld t player
    | null t = []
    | ((snd sld) == (snd (head t))) && (((fst (head t))) == D) = (snd (head t)) : filterSlideEndingTiles sld (tail t) player
    | otherwise = filterSlideEndingTiles sld (tail t) player


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

boardEvaluator :: Piece -> Grid -> Int -> Board -> Bool -> Int
-- This is a very basic heuristic. Just check number of pieces
-- and who won the game.
-- As this function will be called exponentially many times, I
-- decided to leave it simple.
boardEvaluator player grid n board myTurn
    | won       = 100
    | lost      = -100
    | otherwise = crunchCount * 25
            where
                crunchCount = (countPieces board player) - (countPieces board (swapPlayer player))
                won = countPieces board (swapPlayer player) < n
                lost = countPieces board player < n

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
minimax (Node _ b children) heuristic
    -- if there is no option, the decision in made
    | null children = b
    | otherwise =
        -- it returns the board that maximizes the given heuristic from all children
        -- to find that, it calls minimax', which will build the value bottom-up
        -- call minimax' with False maxPlayer because the first level is dealt here
        -- using maximum. So the next level will be my opponent's move, which I
        -- want to minimize.
        let valueList = map (\e -> (minimax' e heuristic False)) children
            maxValue  = maximum valueList
            index     = (findElem valueList maxValue 0)
        in board (children!!index)
        where
            findElem (x:xs) e acc
                | x == e = acc
                | otherwise = findElem xs e (acc + 1)


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
minimax' (Node _ b children) heuristic maxPlayer
    -- base case, it is a leaf. All leaves should have an heuristic value assigned
    | null children = heuristic b maxPlayer
    | otherwise =
        -- if it is my turn, I want to maximize, if it is my opponent's turn
        -- I want to minimize
        let f = if maxPlayer then maximum else minimum
        -- apply minimax' recursively to each child, swapping players
        -- at each level
        in f (map (\e -> (minimax' e heuristic (not maxPlayer))) children)


-- try: play ["WWWWWW-------BBBBBB"] 'W' 2 3
-- function to play the game
play :: [String] -> Char -> Int -> Int -> IO ()
play history@(current:old) player depth n
  | gameOver (sTrToBoard current) (map sTrToBoard old) n = putStrLn "Game over."
  | otherwise = do
       let history'@(new:_) = crusher history player depth n
       putStrLn $ player:" played: " ++ new
       play history' (if player == 'W' then 'B' else 'W') depth n