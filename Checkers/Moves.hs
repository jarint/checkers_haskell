module Checkers.Moves where

import Checkers.Types
import Data.List



-- instructions from new tutorial notes

-- return all the available legal moves based on the current game state

-- FIRST THING TO DO is to check if its red player's turn or black player's turn
-- by checking the status of the input state

-- if it is the red player's turn
-- list the possible jump moves and simple moves that each piece can do

-- for a Pawn:
-- if your pawn is located in (x, y)
-- then the possible destination by simple moves are:
-- (x + 1, y + 1) and (x - 1, y + 1)
-- check if the destination is not occupied by other piece
-- AND the destination is inside the board (x <= 7, x >= 0, y <= 7, y >= 0)
-- 

-- for a King:
-- if your king is located in (x, y)
-- then possible destinations by simple moves are:
-- (x + 1, y + 1), (x + 1, y - 1), (x - 1, y + 1), (x - 1, y - 1)
-- check if the destination is not occupied by other piece
-- AND the destination is inside the board (x <= 7, x >= 0, y <= 7, y >= 0)
-- You need to check an extra condition for a king's move
-- a king's move can cause a repeated game state (which can lead to a loop)

-- history = [[K(2, 1), K(1, 2)], [K (1, 0), K(0, 1)], [K(1, 2), K(2, 1)]]
-- illegal move = [K (0, 1), K(1, 0)]

-- []
-- []

-- take an initially empty list
-- add the coordinates of the new move to the list
-- traverse the history from left to right
-- for each move in the history:
   -- if the coordinates of the move already exist in the list, remove them from the list
   -- if they don't exist in the list add them to the list

-- in the end, if you end up with empty list -> the new move creates a repeated state
-- otherwise it doesn't create a repeated state 


-- Implement your code for moves function below
--moves:: GameState -> (SMorJM [Move])
--moves game = EndM


-- Notes from T02 with Melika

-- returns all available valid moves based on the current state of the game
-- list all the simple and jump moves available for pawns and kings

-- First: check the status of the game state to see if 
    -- it is red players turn or black players turn
    -- example: it is red players turn

-- Second: Check the simple moves available for a red pawn
-- If the red pawn is in (x, y):
    -- (x + 1, y + 1), (x - 1, y + 1)
    -- iff they are not occupied and are not outside of the board

    -- isOccupied :: Coord -> [Coord] -> Bool
    -- checks if the input coordinate is occupied or not

    -- isInBoard :: Coord -> Bool
    -- x >= 0, y <= 7, y >= 0, y <= 7

-- Third: Look at the jump moves available for a pawn
    -- (x + 2, y + 2), (x - 2, y + 2)

-- Fourth: Simple moves available for a red King in (x, y)
    -- (x + 1, y + 1), (x + 1, y - 1), (x - 1, y + 1), (x - 1, y - 1)
    -- iff they are not occupied
    -- iff they are inside the board
    -- a king's move can cause a repeated state (which can lead to a loop in the game)
    -- so you have an extra condition which is to check if a move creates a repeated state
    -- in the game or not.


-- HANDLING REPEATED STATES

-- Suppose some moveseet [K (xx, yy)] --- -- - -- - repeated moveset that causes a repeated state
-- [K(0,1) K(1, 0)] ends up being illegal because it repeats a state

-- 1. []
-- 2. [K (0,1), K (1,0)]
-- 3. [K (0,1), K (1,0), K (2,1)]
-- 4. [K (0,1), K (1,0), K (2,1), K(1,2)]
-- 5. [K (0,1), K (2,1), K(1,2)]
-- 6. [K (2,1), K(1,2)]
-- 7. [K (2,1)]
-- 8. []


-- take a list, which is initially empty
-- list = []
-- add the new move to that list
-- traverse the history from left to right
-- for each move in the history:
    -- if one of the coordinates of the move is currently in the list,
        -- remove it from the list
    -- if not in the list
        -- add it to the list
-- if you end up with an empty list, then it means that you have a repeated state
    -- and the move should be illegal
--otherwise the move is legal





-- Utilities
direction :: GameState -> Int
direction g = if status g == RedPlayer then -1 else 1

jumpOver :: [[a]] -> [[a]]
jumpOver [] = [[]]
jumpOver z  = z

isInBoard :: Coord -> Bool
isInBoard (x, y) = x >= 0 && x <= 7 && y >= 0 && y <= 7

isFreeSquare :: Coord -> GameState -> Bool
isFreeSquare c g = not (occupied c (redPieces g ++ redKings g ++ blackPieces g ++ blackKings g))

isOpponentSquare :: Coord -> GameState -> Bool
isOpponentSquare c g = c `elem` (if status g == RedPlayer then blackPieces g ++ blackKings g else redPieces g ++ redKings g)

occupied :: Coord -> [Coord] -> Bool
occupied c coords = c `elem` coords

getCoord :: PorK Coord -> Coord
getCoord (P c) = c
getCoord (K c) = c

isMoveByKing :: Move -> Bool
isMoveByKing (K _ : _) = True
isMoveByKing _         = False


-- Move Generation
moves :: GameState -> SMorJM [Move]
moves st
  | not (null jumpMovesInSt)   = JM jumpMovesInSt
  | not (null simpleMovesInSt) = SM simpleMovesInSt
  | otherwise                  = EndM
  where
    simpleMovesInSt = simpleMoves st
    jumpMovesInSt   = jumpMoves st


-- HISTORY TRACKING
noCycle :: Move -> [Move] -> Bool
noCycle = detectMoveCycle []

detectMoveCycle :: [PorK Coord] -> [PorK Coord] -> [Move] -> Bool
detectMoveCycle [] [] _  = False
detectMoveCycle _  _  [] = True
detectMoveCycle prevSet currSet (m:ms) =
  typeIsJumpMove m || not (isMoveByKing m) || detectMoveCycle currSet (updateCycleTracker m prevSet) ms

updateCycleTracker :: Move -> [PorK Coord] -> [PorK Coord]
updateCycleTracker [start, end] = flipPiecePosition end . flipPiecePosition start
updateCycleTracker _ = id

flipPiecePosition :: PorK Coord -> [PorK Coord] -> [PorK Coord]
flipPiecePosition x [] = [x]
flipPiecePosition x (y:ys)
  | x == y    = ys
  | otherwise = y : flipPiecePosition x ys






-- SIMPLE MOVES
simpleMoves :: GameState -> [Move]
simpleMoves st = case status st of
  RedPlayer   -> simpleKing (redKings st) st (history st) ++ simplePiece (redPieces st) (allBlackPieces st) st
  BlackPlayer -> simpleKing (blackKings st) st (history st) ++ simplePiece (blackPieces st) (allRedPieces st) st
  _           -> []
  where
    allBlackPieces = (++ blackPieces st) . blackKings
    allRedPieces = (++ redPieces st) . redKings

simplePiece :: [Coord] -> PieceState -> GameState -> [Move]
simplePiece coords opponents st =
  [ [P (x, y), promoteIfEnd (x', y')]
  | (x, y) <- coords
  , let dy = direction st
  , (x', y') <- [(x + 1, y + dy), (x - 1, y + dy)]
  , not (occupied (x', y') opponents) && isInBoard (x', y')
  ]
  where
    promoteIfEnd c@(_, y') = if y' == 0 || y' == 7 then K c else P c

simpleKing :: [Coord] -> GameState -> [Move] -> [Move]
simpleKing kings st hist =
  [ [K (x, y), K (x', y')]
  | (x, y) <- kings
  , (x', y') <- [(x + 1, y + 1), (x - 1, y + 1), (x + 1, y - 1), (x - 1, y - 1)]
  , isInBoard (x', y')
  , isFreeSquare (x', y') st
  , noCycle [K (x, y), K (x', y')] hist
  ]






-- JUMP MOVES
typeIsJumpMove :: Move -> Bool
typeIsJumpMove [start, end] = abs (fst (getCoord start) - fst (getCoord end)) /= 1
typeIsJumpMove _           = True

jumpMoves :: GameState -> [Move]
jumpMoves st = case status st of
  RedPlayer   -> jumpKing (redKings st) st ++ jumpPawn (redPieces st) st
  BlackPlayer -> jumpKing (blackKings st) st ++ jumpPawn (blackPieces st) st
  _           -> []

jumpKing :: [Coord] -> GameState -> [Move]
jumpKing coords st =
  [ K (x, y) : rest
  | (x, y) <- coords
  , rest <- jumpKingHelper (x, y) [] (x, y) st
  ]

jumpKingHelper :: Coord -> [Coord] -> Coord -> GameState -> [Move]
jumpKingHelper start visited (x, y) st =
  [ K (x'', y'') : rest
  | ((cx, cy), (x'', y'')) <- [((x + 1, y + 1), (x + 2, y + 2)), ((x - 1, y + 1), (x - 2, y + 2)), ((x + 1, y - 1), (x + 2, y - 2)), ((x - 1, y - 1), (x - 2, y - 2))]
  , not (elem (cx, cy) visited), isOpponentSquare (cx, cy) st
  , isInBoard (x'', y'')
  , isFreeSquare (x'', y'') st || (x'', y'') == start
  , rest <- jumpOver (jumpKingHelper start ((cx, cy):visited) (x'', y'') st)
  ]

jumpPawn :: [Coord] -> GameState -> [Move]
jumpPawn coords st =
  [ P (x, y) : landingMoves
  | (x, y) <- coords
  , landingMoves <- jumpPawnHelper (x, y) [] (x, y) st
  ]

jumpPawnHelper :: Coord -> [Coord] -> Coord -> GameState -> [Move]
jumpPawnHelper start visited (x, y) st =
  [ pieceType (x'', y'') : rest
  | let dy = direction st
  , let dy2 = 2 * dy
  , ((cx, cy), (x'', y'')) <- [((x + 1, y + dy), (x + 2, y + dy2)), ((x - 1, y + dy), (x - 2, y + dy2))]
  , not (elem (cx, cy) visited), isOpponentSquare (cx, cy) st
  , isInBoard (x'', y'')
  , isFreeSquare (x'', y'') st || (x'', y'') == start
  , rest <- jumpOver (if y'' == 0 || y'' == 7
                       then jumpKingHelper start ((cx, cy):visited) (x'', y'') st
                       else jumpPawnHelper start ((cx, cy):visited) (x'', y'') st)
  ]
  where pieceType c@(_, y'') = if y'' == 0 || y'' == 7 then K c else P c
