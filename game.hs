module Game where

    -- Entry point is 'play'

    -- helpers in place and somewhat tested, but have been experimenting with 'Maybe' for exception handling and running into errors when trying to sync up with IO for game playing.

    -- to simplify, maybe we should just consider an IO game that either reaches the goal, or hits a game over on user input rather than a solver

import Data.Array
import Data.Maybe
import Control.Monad

data Tile = Invalid | Empty | Peg
    deriving (Eq, Ord)

data Direction = Up | Down | Left | Right
    deriving (Enum, Show)

newtype Board = Board (Array (Int, Int) Tile)
    deriving (Eq, Ord)

instance Show Tile where
    show t = tileToString(t)

instance Show Board where
    show (Board board) = unlines [unwords [show (board ! (x, y)) | x <- [0..6]] | y <- [0..6]]

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

newPositions (x,y) Up  = ((x,y+1), (x,y+2))
newPositions (x,y) Game.Right   = ((x+1,y), (x+2,y))
newPositions (x,y) Game.Down  = ((x,y-1), (x,y-2))
newPositions (x,y) Game.Left   = ((x-1,y), (x-2,y))

makeMove :: (Int, Int) -> Direction -> Board -> Maybe Board
makeMove position direction board = do
    Peg <- pegAt position board
    let (over, destination) = newPositions position direction
    Peg <- pegAt over board
    Empty <- pegAt destination board

    return  . placePiece position Empty 
            . placePiece over Empty  
            . placePiece destination Peg 
            $ board

play = do
    putStr "Time to play the game!\n"
    playGame initialBoard

playGame board = do
    putStr (show board)
    putStr "Make a move:\n"
    putStr "X: "
    x <- getLine
    putStr "Y: "
    y <- getLine
    let xInt = read (x) :: Int
    let yInt = read (y) :: Int
    -- putStr "Direction: \n"
    let nextBoard = makeMove (xInt,yInt) Up board
    if (nextBoard == Nothing) then putStr "invalid move\n" 
    else putStr "valid move\n"