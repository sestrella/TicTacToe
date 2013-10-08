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
slices b = (diagonalSlices b) ++ (verticalSlices b) ++ b

verticalSlices :: Board -> [[Maybe Piece]]
verticalSlices b = map (\column -> map (\row -> row !! column) b) [0..2]

diagonalSlices :: Board -> [[Maybe Piece]]
diagonalSlices b = map(\n -> 
                      map (\index -> b !! index !! abs (index - n)) 
                          [0..2]) 
                      [0, 2]

sliceWinner :: [Maybe Piece] -> Maybe Player
sliceWinner slices = undefined

--winnerRow :: [[Maybe Piece]] -> [Maybe Piece]
--winnerRow slices =

samePiece :: [Maybe Piece] -> Maybe Piece
-- [Cross, Cross, Cross] -> Cross
-- [Cross, Circle, Cross] -> Nothing
samePiece slice =
  let f = head slice in
    foldl (\n r -> case n of
            Nothing -> Nothing
            Just r  -> (if r == f then r else Nothing)) 
          f 
          slice

playerFromPiece :: Piece -> Player
playerFromPiece p 
    | Cross  == p  = Player1
    | Circle == p  = Player2 

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
