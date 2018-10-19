module Game where

    -- Entry point is 'play'

    -- helpers in place and somewhat tested, but have been experimenting with 'Maybe' for exception handling and running into errors when trying to sync up with IO for game playing.

    -- to simplify, maybe we should just consider an IO game that either reaches the goal, or hits a game over on user input rather than a solver

import Data.List
import Data.Array
import Data.Maybe
import Control.Monad
import Text.Read

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
     

getState :: Board -> State
getState board
    |winGame board = Win
    |loseGame board = Lose
    |otherwise = Continue

winGame :: Board -> Bool
winGame (Board b) = ((length [t| t<- elems b,t==Peg ])==1 && fromJust(pegAt(3,3) (Board b))==Peg)

easyWinGame :: Board -> Bool
easyWinGame (Board b) = ((length [t| t<- elems b,t==Peg ])==1)


loseGame :: Board -> Bool
loseGame (Board b) = length (possiblePlayOnBoard (Board b)) == 0

possiblePlayOnBoard :: Board -> [Action]
possiblePlayOnBoard (Board b) = foldl (++) [] [possiblePlayOnPos pos (Board b)|pos<-indices b]

possiblePlayOnPos :: (Int,Int) -> Board -> [Action]
possiblePlayOnPos position board = [Action (position, dir) |dir <- [Up .. ],isJust (makeMove (Action (position,dir)) board)]

tileToString :: Tile -> String
tileToString t
    | t == Invalid = " "
    | t == Empty = "o"
    | otherwise = "i"

boardToArray :: Board -> Array (Int, Int) Tile
boardToArray (Board board) = board

isValidLocation :: (Int, Int) -> Bool
isValidLocation (x,y) =
    x >= 0 && x <= 6 &&
    y >= 0 && y <= 6 &&
    ((x >= 2 && x <= 4) ||
     (y >= 2 && y <= 4))

isInvalidLocation :: (Int, Int) -> Bool
isInvalidLocation = not . isValidLocation

initializeLocations :: (Int, Int) -> Tile
initializeLocations (3,3) = Empty
initializeLocations position
  | isInvalidLocation position = Invalid
  | otherwise = Peg


initialBoard :: BoardType -> Board
initialBoard t = Board (array ((0,0), (6,6)) [ (location, initializeLocations location) | location <- allLocations ])

allLocations :: [(Int,Int)]
allLocations = range ((0,0), (6,6))

pegAt :: (Int, Int) -> Board -> Maybe Tile
pegAt position (Board board) = do
    guard (isValidLocation position) 
    return (board ! position)

placePiece :: (Int, Int) -> Tile -> Board -> Board
placePiece position tile (Board board) = Board (board // [(position, tile)])

newPositions (x,y) Down  = ((x,y+1), (x,y+2))
newPositions (x,y) Game.Right   = ((x+1,y), (x+2,y))
newPositions (x,y) Game.Up  = ((x,y-1), (x,y-2))
newPositions (x,y) Game.Left   = ((x-1,y), (x-2,y))

makeMove :: Action -> Board -> Maybe Board
makeMove (Action (position, direction)) board = do
    recent <- pegAt position board
    let (over, destination) = newPositions position direction
    o <- pegAt over board
    des <- pegAt destination board
    if (recent==Peg&&o==Peg&&des==Empty) then
        return  . placePiece position Empty 
                . placePiece over Empty  
                . placePiece destination Peg 
                $ board
    else do
        Nothing

askFor :: String -> IO Int
askFor s = do
    putStr (s++": ")
    x <- getLine
    let xMaybeInt = readMaybe (x)
    let xInt = fromMaybe (-1) xMaybeInt
    if (xInt<0||xInt>6) then do
        putStrLn("Your choise of position is illegal\nPlease choose again")
        askFor s
    else
        (return xInt)
askForDir :: (Int, Int) -> Board -> IO Direction
askForDir (x,y) board = do
    putStrLn("Choose a direction")
    --putStrLn(show (possiblePlayOnPos (x,y) board))
    d <- getLine
    --let dString = read (d)::String
    let dDir = (stringToDirection d)
    if (isNothing dDir) then do
        putStrLn("invalid direction")
        askForDir (x,y) board
    else return (fromJust dDir)

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

solve :: Board -> (State, [Action])
solve board = do 
    let moves = possiblePlayOnBoard board
    solveHelper moves board []

testing = solveHelper(possiblePlayOnBoard (initialBoard English)) (initialBoard English) []
solveHelper :: [Action] -> Board -> [Action] -> (State, [Action])
solveHelper moves board movesSoFar = do
    if (winGame board) then
        (Win, movesSoFar)
    else if (length moves == 0) then
        (Lose, [])
    else do
        let currentMove = moves !! 0
        let nextBoard = fromJust(makeMove currentMove board)
        let (nextResult,as) = solveHelper (possiblePlayOnBoard nextBoard)  nextBoard (movesSoFar++[currentMove])
        if (nextResult==Win) then 
            (Win,as)
        else
            (solveHelper (delete (moves !! 0) moves) board movesSoFar)

            
testingWithIO = solveHelperDebug (possiblePlayOnBoard (initialBoard English)) (initialBoard English) [] 0 
lostBoard :: [Board]
lostBoard = []

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

testingMem = solveHelperDebugMem (possiblePlayOnBoard (initialBoard English)) (initialBoard English) [] 0 []
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



cheaterCheck :: Action -> Bool
cheaterCheck (Action ((x,y), d)) = (x==0&&y==0&&d==Up)


play :: IO ()
play = do
    putStrLn "Time to play the game!\n(0,0) is top corner"
    putStrLn "0-6 for axis\nup/down/left/right for direction"
    (result,actions) <- (playGame (initialBoard English) Continue [])

    putStrLn("You "++(show result))
    putStrLn("Your actions: " ++ (show actions))

playGame :: Board -> State -> [Action]-> IO (State, [Action])
playGame board state actions= do
    putStrLn ("Current board:")
    putStr (show board)
    action <- askForAction board
    let nextBoard = makeMove action board
    if (cheaterCheck action) then do
        putStrLn("You called the solver\nIt is doing the work for you")
        let (resultState,computedActions) = solve board
        return (resultState, actions++computedActions)
    else if (nextBoard == Nothing) then do
        putStrLn("Previous move is illegal")
        playGame board state actions

    else do
        let b = (fromJust nextBoard)
        let s = getState b
        if (s == Win||s == Lose) then 
            return (s, (actions++[action]))
        else    
            playGame b s (actions++[action])
        






