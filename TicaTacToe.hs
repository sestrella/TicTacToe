import qualified Data.Maybe    as MB
import qualified System.Random as R

data Player = Player1 | Player2 
              deriving(Eq, Show)

data Piece = Cross | Circle
             deriving(Eq, Show)

type Position =  (Int, Int)

type Board = [[Maybe Piece]]

-- Boards
emptyBoard :: Board
emptyBoard = replicate 3 (replicate 3 Nothing)

fullBoard :: Board
fullBoard = [[Just Circle, Just Cross, Just Cross],
             [Just Circle, Just Circle, Just Cross],
             [Just Cross, Just Cross, Just Circle]]

winnerBoard :: Board
winnerBoard = [[Just Cross, Nothing, Nothing],
              [Nothing, Just Cross, Nothing],
              [Nothing, Nothing, Just Cross]]

-- Slicing functions
slices :: Board -> [[Maybe Piece]]
slices b = (diagonalSlices b) ++ (verticalSlices b) ++ b

verticalSlices :: Board -> [[Maybe Piece]]
verticalSlices b = map (\column -> map (\row -> row !! column) b) [0..2]

diagonalSlices :: Board -> [[Maybe Piece]]
diagonalSlices b = map(\n -> 
                      map (\index -> b !! index !! abs (index - n)) 
                          [0..2]) 
                      [0, 2]

-- Pieces funtions
samePiece :: [Maybe Piece] -> Maybe Piece
-- [Cross, Cross, Cross] -> Cross
-- [Cross, Circle, Cross] -> Nothing
samePiece slice
  | allSamePiece (head slice) slice = head slice
  | otherwise = Nothing

allSamePiece :: Maybe Piece -> [Maybe Piece] -> Bool
allSamePiece h s = all (\n -> n == h) s

playerFromPiece :: Maybe Piece -> Maybe Player
playerFromPiece p 
    | Just Cross  == p  = Just Player1
    | Just Circle == p  = Just Player2
    | otherwise = Nothing

pieceFromPlayer :: Maybe Player -> Maybe Piece
pieceFromPlayer p 
    | Just Player1 == p  = Just Cross
    | Just Player2 == p  = Just Circle
    | otherwise = Nothing

-- Ending functions
winner :: [[Maybe Piece]] -> Maybe Player
winner s = foldl (\r n -> 
                  if r == Nothing then playerFromPiece (samePiece n) else r)
                  Nothing 
                  s

isFinished :: Board -> Bool
isFinished b
  | isFilled b = True
  | winner (slices b) /= Nothing = True
  | otherwise = False


isFilled :: Board -> Bool
isFilled b = any (\row -> row /= Nothing) (concat b)    

-- 
placePiece :: Board -> Maybe Piece -> Position -> Board
placePiece b piece (x, y) = map (\y' -> map (\x' -> 
                                 if x' == x && y' == y
                                 then piece else b !! y' !! x') 
                                 [0..2]) [0..2]

turn :: Board -> Maybe Player -> Board
turn b p = placePiece b (pieceFromPlayer p) (randomPosition b)

game :: Board -> Maybe Player -> Maybe Board
game b p = if isFinished b then 
              Nothing
            else
              game (turn b p) (switchPlayer p)

--showBoard :: IO()
--showBoard = game emptyBoard (Just Player1)

-- TODO: Implement
randomPosition :: Board -> Position
randomPosition b = (1, 1)

switchPlayer :: Maybe Player -> Maybe Player
switchPlayer p
  | p == Just Player1 = Just Player2
  | p == Just Player2 = Just Player1
  | otherwise = Nothing