module Checkers.Types where

import Data.Maybe

{-
    A coordinate represents a square on the board, it is a pair of integers (x,y).
    The board is 0-indexed, where (_,0) is the top row of the board, and (0,_) is the left-most column.
    The state of the board is given as a list of coordinates.
-}
type Coord = (Int, Int)
type PieceState = [Coord]
{-
    A move is essentially a list of coordinates, tracing our the path travelled by a piece, where you keep track of whether or not the piece is a Pawn or King using the PorK datatype.
    The list goes "in the right order", e.g. [firstSquare, secondSquare, ...]
-}
data PorK a = P a | K a  
  deriving (Show,Eq, Read)

type Move = [PorK Coord]

{-
    The player datatype is the red/black colour.
-}
data Player = Red | Black -- 2 players, red and black
  deriving (Eq, Show)
{-
    It is either red/black's turn, or the game has finished.
-}
data Status = Turn Player | GameOver
  deriving (Eq,Show)
-- The status determines whether or not the game is still ongoing
-- Winner Nothing is a tie

-- Gamestate has al of the data for the game.
data GameState =
  GameState { blackPieces :: PieceState
            , redPieces :: PieceState
            , blackKings :: PieceState
            , redKings :: PieceState
            , status :: Status
            , message :: String 
            , history :: [Move]}
              deriving (Show, Eq)


-- Your job is to write two functions of these types

type CheckersEngine = Move -> GameState -> GameState
type GenMove = GameState -> Move



-- The initial game state

initialGameState :: GameState
initialGameState =
  GameState { blackPieces = blackInit
            , redPieces = redInit
            , blackKings = []
            , redKings = []
            , status = Turn Red
            , message = ""
            , history = []}


testGameState :: GameState
testGameState = 
    GameState {blackPieces = [],
               redPieces = [],
               blackKings = [(1,0)],
               redKings = [(0,3),(2,3)], 
               status = Turn Black,
               message = "", 
               history = [[K (1,2),K (0,3)],[K (0,1),K (1,0)],[K (0,3),K (1,2)]]}

testGameState2 :: GameState
testGameState2 = 
    GameState {blackPieces = [],
               redPieces = [],
               blackKings = [(1,1)],
               redKings = [(5,5)],
               status = Turn Black,
               message = "",
               history = [[K (5,5),K (4,4)],[K (0,0),K (1,1)],[K (4,4),K (5,5)]]}

blackInit :: [Coord]
blackInit = [ (1,0), (3,0), (5,0), (7,0)
            , (0,1), (2,1), (4,1), (6,1)
            , (1,2), (3,2), (5,2), (7,2)]

redInit :: [Coord]
redInit = [ (0,7), (2,7), (4,7), (6,7)
          , (1,6), (3,6), (5,6), (7,6)
          , (0,5), (2,5), (4,5), (6,5)]

