module Game where

import Data.List
import Data.Array
import Data.Maybe
import Control.Monad
import Text.Read


 
--------Data type declarations----------------

data Tile = Invalid | Empty | Peg
    deriving (Eq, Ord)

data Direction = Up | Down | Left | Right
    deriving (Enum, Show,Eq)

data BoardType = English | European
    deriving (Enum, Show, Eq)

data State = Win | Lose | Continue
    deriving (Eq, Show)

data Action = Action ((Int,Int), Direction)
    deriving (Eq)

newtype Board = Board (Array (Int, Int) Tile)
    deriving (Eq, Ord)

instance Show Tile where
    show t = tileToString(t)

instance Show Board where
    show (Board board) = unlines [unwords [show (board ! (x, y)) | x <- [0..6]] | y <- [0..6]]

instance Show Action where
    show (Action ((x,y), d)) = "("++show (x,y) ++", "++ show (d)++")"
     

--------- Game State Control ---------

getState :: Board -> State
getState board
    |winGame board = Win
    |loseGame board = Lose
    |otherwise = Continue

-- The game is  won if only one peg remains, and it is located in the center of the board.
winGame :: Board -> Bool
winGame (Board b) = ((length [t| t<- elems b,t==Peg ])==1 && fromJust(pegAt(3,3) (Board b))==Peg)

-- helper function with a simpler win condition for testing purposes
easyWinGame :: Board -> Bool
easyWinGame (Board b) = ((length [t| t<- elems b,t==Peg ])==1)


loseGame :: Board -> Bool
loseGame (Board b) = length (possiblePlayOnBoard (Board b)) == 0


--------- Game Initialization & Helpers ---------

tileToString :: Tile -> String
tileToString t
    | t == Invalid = " "
    | t == Empty = "o"
    | otherwise = "i"

boardToArray :: Board -> Array (Int, Int) Tile
boardToArray (Board board) = board

-- ensure a position is within the bounds of the English game board
isValidLocation :: (Int, Int) -> Bool
isValidLocation (x,y) =
    x >= 0 && x <= 6 &&
    y >= 0 && y <= 6 &&
    ((x >= 2 && x <= 4) ||
     (y >= 2 && y <= 4))

isInvalidLocation :: (Int, Int) -> Bool
isInvalidLocation = not . isValidLocation

-- initialize game board with pegs in all slots but the center
initializeLocations :: (Int, Int) -> Tile
initializeLocations (3,3) = Empty
initializeLocations position
  | isInvalidLocation position = Invalid
  | otherwise = Peg


initialBoard :: BoardType -> Board
initialBoard t = Board (array ((0,0), (6,6)) [ (location, initializeLocations location) | location <- allLocations ])

allLocations :: [(Int,Int)]
allLocations = range ((0,0), (6,6))


--------- Peg Movement Functions ---------

-- helper func to quickly determine the status of a specified board tile
pegAt :: (Int, Int) -> Board -> Maybe Tile
pegAt position (Board board) = do
    guard (isValidLocation position) 
    return (board ! position)

-- set the status of the specified position on a board to a given tile type
placePiece :: (Int, Int) -> Tile -> Board -> Board
placePiece position tile (Board board) = Board (board // [(position, tile)])

-- Determine all possible moves from a given board
possiblePlayOnBoard :: Board -> [Action]
possiblePlayOnBoard (Board b) = foldl (++) [] [possiblePlayOnPos pos (Board b)|pos<-indices b]

-- determine a list of valid moves from a given position on a given board state.
possiblePlayOnPos :: (Int,Int) -> Board -> [Action]
possiblePlayOnPos position board = [Action (position, dir) |dir <- [Up .. ],isJust (makeMove (Action (position,dir)) board)]

-- A given action produces either a new valid game state, or nothing following an invalid move
makeMove :: Action -> Board -> Maybe Board
makeMove (Action (position, direction)) board = do
    -- attempt to complete the specified action
    recent <- pegAt position board
    let (over, destination) = newPositions position direction
    o <- pegAt over board
    des <- pegAt destination board

    -- ensure the action complies with the rules of the game
    if (recent==Peg&&o==Peg&&des==Empty) then
        return  . placePiece position Empty 
                . placePiece over Empty  
                . placePiece destination Peg 
                $ board
    else do
        Nothing

-- New coordinates of a given position after a move
-- new position contains both the destination peg as well as the peg jumped over to reach it
newPositions :: (Int, Int) -> Direction -> ((Int, Int), (Int, Int))
newPositions (x,y) Down  = ((x,y+1), (x,y+2))
newPositions (x,y) Game.Right   = ((x+1,y), (x+2,y))
newPositions (x,y) Game.Up  = ((x,y-1), (x,y-2))
newPositions (x,y) Game.Left   = ((x-1,y), (x-2,y))


--------- IO Prompts ---------

askFor :: String -> IO Int
askFor s = do
    putStr (s++": ")
    x <- getLine
    let xMaybeInt = readMaybe (x)
    let xInt = fromMaybe (-1) xMaybeInt
    if (xInt<0||xInt>6) then do
        putStrLn("Your choice of position is illegal\nPlease choose again\n")
        askFor s
    else
        (return xInt)

askForDir :: (Int, Int) -> Board -> IO Direction
askForDir (x,y) board = do
    putStrLn("Choose a direction")
    d <- getLine
    let dDir = (stringToDirection d)
    if (isNothing dDir) then do
        putStrLn("invalid direction")
        askForDir (x,y) board
    else return (fromJust dDir)

-- helper function required to translate user input to a game direction type
stringToDirection :: String -> Maybe Direction
stringToDirection s
    | s == "up"|| s == "u" =  Just Up
    | s == "down"|| s == "d" = Just Down   
    | s == "left"|| s == "l" = Just Game.Left
    | s == "right"|| s == "r" = Just Game.Right
    | otherwise = Nothing

askForAction :: Board -> IO Action
askForAction board = do
    putStr "Make a move:\n"
    x <- askFor "X" 
    y <- askFor "Y" 
    dir <- (askForDir (x,y) board)
    return (Action ((x,y),dir))

--------- Game Solvers ---------

-- hidden cheat allows the user to automatically solve the game if they desire
-- required input X:0, Y:0, Direction: up
cheaterCheck :: Action -> Bool
cheaterCheck (Action ((x,y), d)) = (x==0&&y==0&&d==Up)

-- entry point for auto solver
solve :: Board -> (State, [Action])
solve board = do 
    let moves = possiblePlayOnBoard board
    solveHelperMem moves board [] []

-- predefined solver call for debugging purposes
testing = solveHelper(possiblePlayOnBoard (initialBoard English)) (initialBoard English) []

-- initial slow solver function without memoization
solveHelper :: [Action] -> Board -> [Action] -> (State, [Action])
solveHelper moves board movesSoFar = do
    if (winGame board) then
        (Win, movesSoFar)
    else if (length moves == 0) then
        (Lose, [])
    else do     -- the current board is in continue state

        -- the next board to be checked is the first off of the stack
        let currentMove = moves !! 0    
        let nextBoard = fromJust(makeMove currentMove board)

        -- recurse with the next board off of the stack, and add its children on to the stack
        let (nextResult,as) = solveHelper (possiblePlayOnBoard nextBoard)  nextBoard (movesSoFar++[currentMove])
        if (nextResult==Win) then 
            (Win,as)
        else
            (solveHelper (delete (moves !! 0) moves) board movesSoFar)

-- predefined solver call for debugging purposes
testingMem = solveHelperMem(possiblePlayOnBoard (initialBoard English)) (initialBoard English) [] []

-- memoized version of solveHelper above
-- passing a list of visited boards that fail cuts solve time in half
solveHelperMem :: [Action] -> Board -> [Action] -> [Board] -> (State, [Action])
solveHelperMem moves board movesSoFar losingBoards = do
    if (elem board losingBoards) then do
        (Lose, movesSoFar)
    else if (winGame board) then
        (Win, movesSoFar)
    else if (length moves == 0) then do
        let losingBoards = board:losingBoards
        (Lose, movesSoFar)
    else do
        let currentMove = moves !! 0
        let nextBoard = fromJust(makeMove currentMove board)
        let (nextResult,as) = solveHelperMem (possiblePlayOnBoard nextBoard)  nextBoard (movesSoFar++[currentMove]) losingBoards
        if (nextResult==Win) then 
            (Win,as)
        else
            (solveHelperMem (delete (moves !! 0) moves) board movesSoFar losingBoards)


--------- Game Entry Point ---------

-- Begins the game prompt
play :: IO ()
play = do
    putStrLn "Time to play the game!\n(0,0) is top corner"
    putStrLn "0-6 for axis\nup/down/left/right for direction"
    (result,actions) <- (playGame (initialBoard English) Continue [])

    putStrLn("You "++(show result))
    putStrLn("Your actions: " ++ (show actions))

-- User interaction point to play the game by responding to IO
-- the game displays the board state at each turn and tracks user moves to be displayed at the end
playGame :: Board -> State -> [Action]-> IO (State, [Action])
playGame board state actions= do
    putStrLn ("Current board:")
    putStr (show board)
    action <- askForAction board
    let nextBoard = makeMove action board

    -- listen for auto solve cheat
    if (cheaterCheck action) then do
        let (resultState,computedActions) = solve board
        return (resultState, actions++computedActions)
    
    else if (nextBoard == Nothing) then do
        -- given action is not valid at the current game state   
        putStrLn("Previous move is illegal")
        playGame board state actions

    else do
        -- given move is valid
        let b = (fromJust nextBoard)
        let s = getState b
        if (s == Win||s == Lose) then 
            return (s, (actions++[action]))
        else    
            playGame b s (actions++[action])
        


--------- Debugging Functions ---------
-- these functions are IO versions of the solvers above used for logging purposes

testingWithIO = solveHelperDebug (possiblePlayOnBoard (initialBoard English)) (initialBoard English) [] 0 

solveHelperDebug:: [Action] -> Board -> [Action] -> Int -> IO (State,[Action])
solveHelperDebug moves board movesSoFar depth = do
    putStrLn(" ")
    putStrLn("At depth: " ++ (show depth))
    --putStrLn("Before: ")
    --putStrLn(show board)
    if (length moves == 0) then do
        putStrLn("out of possible moves")
        return (Lose, movesSoFar)
    else do
        let firstMove = moves !! 0
        --putStrLn("move: " ++ show firstMove)
        let nextBoard = fromJust (makeMove firstMove board)
        --putStrLn("After: ")
        putStrLn(show nextBoard)
        if(elem nextBoard lostBoard) then do
            putStrLn("have seen this before and lost")
            return (Lose,movesSoFar)
        else do
            (nextResult,as)<- solveHelperDebug (possiblePlayOnBoard nextBoard)  nextBoard (movesSoFar++[firstMove]) (depth+1) 
            if (nextResult==Win) then 
                return (Win,as)
            else do
                solveHelperDebug (delete (moves !! 0) moves) board movesSoFar depth 

lostBoard :: [Board]
lostBoard = []
-- testingMem = solveHelperDebugMem (possiblePlayOnBoard (initialBoard English)) (initialBoard English) [] 0 []
solveHelperDebugMem:: [Action] -> Board -> [Action] -> Int -> [Board]-> IO ((State,[Action]),[Board])
solveHelperDebugMem moves board movesSoFar depth lostBoards = do
    putStrLn(" ")
    putStrLn("At depth: " ++ (show depth))
    --putStrLn("Before: ")
    --putStrLn(show board)
    putStrLn(show(length lostBoards))
    if (length moves == 0) then do
        putStrLn("out of possible moves")
        return ((Lose, movesSoFar),union lostBoards [board])
    else do
        let firstMove = moves !! 0
        --putStrLn("move: " ++ show firstMove)
        let nextBoard = fromJust (makeMove firstMove board)
        --putStrLn("After: ")
        putStrLn(show nextBoard)
        if(elem nextBoard lostBoards) then do
            putStrLn("have seen this before and lost")
            return ((Lose,movesSoFar),lostBoards)
        else do
            ((nextResult,as),lbs) <- solveHelperDebugMem (possiblePlayOnBoard nextBoard)  nextBoard (movesSoFar++[firstMove]) (depth+1) lostBoards
            if (nextResult==Win) then 
                return ((Win,as),[])
            else do
                (solveHelperDebugMem (delete (moves !! 0) moves) board movesSoFar depth (union lostBoards lbs))
