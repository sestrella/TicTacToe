import qualified Data.Maybe as MB
import qualified System.Random as R

data Player = Player1 | Player2 
              deriving(Eq, Show)

data Piece = Cross | Circle
             deriving(Eq, Show)

type Board = [[Maybe Piece]]

type Position = (Int, Int)

type Round = (Piece, Position)

type Game = [Round]

wonBy :: [Maybe Piece] -> Maybe Player
wonBy slice = undefined

slices :: Board -> [[Maybe Piece]]
slices b = (diagonalOne b):(diagonalTwo b):(verticalSlices b) ++ b

verticalSlices :: Board -> [[Maybe Piece]]
verticalSlices b = map (\column -> map (\row -> row !! column) b) [0..2]

diagonalOne :: Board -> [Maybe Piece]
diagonalOne b = map (\index -> b !! index !! index) [0..2]

diagonalTwo :: Board -> [Maybe Piece]
diagonalTwo b = map (\index -> b !! index !! (2 - index)) [0..2]

pieceForPlayer :: Player -> Piece
pieceForPlayer p  
    | Player1 == p  = Cross
    | Player2 == p  = Circle 

emptyBoard :: Board
emptyBoard = replicate 3 (replicate 3 Nothing)

fullBoard :: Board
fullBoard = [[Just Circle, Just Cross, Just Cross],
             [Just Circle, Just Circle, Just Cross],
             [Just Cross, Just Cross, Just Circle]]

isFinished :: Board -> Bool
isFinished b = undefined

isFilled :: Board -> Bool
isFilled b = any (\row -> row /= Nothing) (concat b)    

winner :: Board -> Maybe Player
winner b = undefined

placePiece :: Board -> Piece -> Position -> Board
placePiece b p (x, y) = undefined

randomPosition :: Board -> Position
randomPosition b = undefined
