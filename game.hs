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

solveHelper :: [Action] -> Board -> [Action] -> (State, [Action])
solveHelper moves board movesSoFar = do
    if (length moves == 0) then do
        (Lose, movesSoFar)
    else do
        let firstMove = moves !! 0
        let nextBoard = makeMove firstMove board
            -- let nextMoves = delete (moves !! 0) moves
            -- solveHelper nextMoves board movesSoFar
        if (getState (fromJust nextBoard) == Continue) then do
            let (state, actions) = solveHelper (possiblePlayOnBoard (fromJust nextBoard)) (fromJust nextBoard) (movesSoFar++[firstMove])
            if (state == Lose) then do
                let restOfMoves = delete (moves !! 0) moves
                solveHelper restOfMoves board movesSoFar
            else (state, actions)
        else if (getState (fromJust nextBoard) == Lose) then do
            (Lose, movesSoFar++[firstMove])
            -- let nextMoves = delete (moves !! 0) moves
            -- solveHelper nextMoves board movesSoFar
        else
            (Win, movesSoFar++[firstMove])
            -- ((getState (fromJust nextBoard)), (movesSoFar++[firstMove]))


            
testing = solveHelperDebug (possiblePlayOnBoard (initialBoard English)) (initialBoard English) [] 0

solveHelperDebug moves board movesSoFar depth = do
    putStrLn(" ")
    putStrLn("At depth: " ++ (show depth))
    putStrLn("Before: ")
    putStrLn(show board)
    if (length moves == 0) then do
        putStrLn("out of possible moves")
        return (Lose, movesSoFar)
    else do
        let firstMove = moves !! 0
        putStrLn("move: " ++ show firstMove)
        let nextBoard = makeMove firstMove board
            -- let nextMoves = delete (moves !! 0) moves
            -- solveHelper nextMoves board movesSoFar
        putStrLn("After: ")
        putStrLn(show (fromJust nextBoard))
        if (getState (fromJust nextBoard) == Continue) then do
            (state, actions) <- solveHelperDebug(possiblePlayOnBoard (fromJust nextBoard)) (fromJust nextBoard) (movesSoFar++[firstMove]) (depth+1)
            if (state == Lose) then do
                let restOfMoves = delete (moves !! 0) moves
                solveHelperDebug restOfMoves board movesSoFar (depth)
            else return (state, actions)
        else if (getState (fromJust nextBoard) == Lose) then do
            putStrLn("deadend, backtracking")
            return (Lose, movesSoFar++[firstMove])
            -- let nextMoves = delete (moves !! 0) moves
            -- solveHelper nextMoves board movesSoFar
        else do
            putStrLn("win state reached")
            return (Win, movesSoFar++[firstMove])
            -- ((getState (fromJust nextBoard)), (movesSoFar++[firstMove]))



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
    --if (cheaterCheck action) then do
        --let (resultState,computedActions) = solve board
        --return (resultState, actions++computedActions)
    if (nextBoard == Nothing) then do
        putStrLn("Previous move is illegal")
        playGame board state actions

    else do
        let b = (fromJust nextBoard)
        let s = getState b
        if (s == Win||s == Lose) then 
            return (s, (actions++[action]))
        else    
            playGame b s (actions++[action])
        






