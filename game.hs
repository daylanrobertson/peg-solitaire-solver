module Game where

    -- Entry point is 'play'

    -- helpers in place and somewhat tested, but have been experimenting with 'Maybe' for exception handling and running into errors when trying to sync up with IO for game playing.

    -- to simplify, maybe we should just consider an IO game that either reaches the goal, or hits a game over on user input rather than a solver

import Data.Array
import Data.Maybe
import Control.Monad
import Text.Read

data Tile = Invalid | Empty | Peg
    deriving (Eq, Ord)

data Direction = Up | Down | Left | Right
    deriving (Enum, Show)

data State = Win | Lose | Continue
    deriving (Eq, Show)

newtype Board = Board (Array (Int, Int) Tile)
    deriving (Eq, Ord)

instance Show Tile where
    show t = tileToString(t)

instance Show Board where
    show (Board board) = unlines [unwords [show (board ! (x, y)) | x <- [0..6]] | y <- [0..6]]

getState :: Board -> State
getState board
    |winGame board = Win
    |loseGame board = Lose
    |otherwise = Continue

winGame :: Board -> Bool
winGame (Board b) = (length [t| t<- elems b,t==Peg ])==1

loseGame :: Board -> Bool
loseGame (Board b) = length [p|p <- indices b,(playable p (Board b))] == 0

playable :: (Int,Int) -> Board -> Bool
playable (x,y) board = (isJust (makeMove (x,y) Up board))||
    (isJust (makeMove (x,y) Down board))||
        (isJust (makeMove (x,y) Game.Left board))||
            (isJust (makeMove (x,y) Game.Right board))

tileToString :: Tile -> String
tileToString t
    | t == Invalid = "x"
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

initialBoard :: Board
initialBoard = Board (array ((0,0), (6,6)) [ (location, initializeLocations location) | location <- allLocations ])

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

makeMove :: (Int, Int) -> Direction -> Board -> Maybe Board
makeMove position direction board = do
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
askForDir :: IO Direction
askForDir = do
    putStrLn("Choose a direction")
    d <- getLine
    --let dString = read (d)::String
    let dDir = (stringToDirection d)
    if (isNothing dDir) then do
        putStrLn("invalid direction")
        askForDir
    else return (fromJust dDir)

stringToDirection :: String -> Maybe Direction
stringToDirection s
    | s == "up"|| s == "u" =  Just Up
    | s == "down"|| s == "d" = Just Down   
    | s == "left"|| s == "l" = Just Game.Left
    | s == "right"|| s == "r" = Just Game.Right
    | otherwise = Nothing

play = do
    putStr "Time to play the game!\n"
    playGame initialBoard Continue

playGame board state
    | state == Win = putStrLn("Congratulation, you win")
    | state == Lose = putStrLn("No more move, you lose")
    | otherwise = do
        putStrLn ("Current board:")
        putStr (show board)
        putStr "Make a move:\n"
        xInt <- askFor "X"
        yInt <- askFor "Y"
        d <- askForDir
        let nextBoard = makeMove (xInt,yInt) d board
        if (nextBoard == Nothing) then do
            putStrLn("Previous move is illegal")
            playGame board state
        else do
            let b = (fromJust nextBoard)
            playGame b (getState b)
        


