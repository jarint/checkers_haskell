module Checkers.ApplyMove where

import Checkers.Moves
import Checkers.Types

------------------------------------------------
------------------------------------------------
--
--  APPLYING A MOVE to a state
--
------------------------------------------------
------------------------------------------------

-- tutorial notes from T02 with Melika:

--apply_move:: Move -> GameState -> GameState
--apply_move mv st = st{message = "!!GAME OVER!!"}


-- First: get all the available moves in the current state
-- using the moves function in the Moves.hs

-- search the input move in the list of available moves
-- if not found --> you set the message fo the game state
-- to something like "Invalid Move"

-- If the move is found:
    -- apply the move by updating the pieces state in the game state
    -- update the status 
    -- clear the message of the game state
    -- add the new move to the history of the game state
    



-- notes from new tutorial upload:

-- get all the possible moves by calling your `moves` function 
-- which you defined in Moves.hs 

-- if the list of valid moves is empty ==> set the status to "Game Over"

-- search for the input move in the list of moves you got 

-- if not found --> set the message of the current state to "Invalid Move"
-- if found -->
    -- 1. update the pieces of the current state
    -- 2. change the status 
    -- 3. clear the message
    -- 4. add the move to the history of the current state






-- helper functions

--extracts the coordinate from either a pawn or king wrapper
unpack_coord :: PorK Coord -> Coord
unpack_coord (P c) = c
unpack_coord (K c) = c

-- checks if it’s currently red’s turn, or
-- checks if it’s currently black's turn
current_RedPlayer :: GameState -> Bool
current_RedPlayer st = status st == RedPlayer
current_BlackPlayer :: GameState -> Bool
current_BlackPlayer st = status st == BlackPlayer

-- removes a piece from a given piece state
remove :: PorK Coord -> PieceState -> PieceState
remove piece = filter (/= unpack_coord piece)

--  replaces one piece with another in a piece state
replace :: PorK Coord -> PorK Coord -> PieceState -> PieceState
replace from to = (:) (unpack_coord to) . filter (/= unpack_coord from)

-- inserts a new piece coordinate into a piece state
insertCoord :: PorK Coord -> PieceState -> PieceState
insertCoord piece = (:) (unpack_coord piece)

-- finds the coordinate of the jumped piece between two squares
capturedSquare :: PorK Coord -> PorK Coord -> Coord
capturedSquare c1 c2 = (startX + stepX, startY + stepY)
  where
    (startX, startY) = unpack_coord c1
    (endX, endY)     = unpack_coord c2
    stepX            = signum (endX - startX)
    stepY            = signum (endY - startY)


-- this is a generic update function for piece states in GameState
-- the four definitions that follow it are shortcuts for 
-- each piece type in the game
updatePieceset :: (GameState -> PieceState) -> (GameState -> PieceState -> GameState) -> (PieceState -> PieceState) -> GameState -> GameState
updatePieceset getter setter op st = setter st (op (getter st))
updateRedKings    = updatePieceset redKings   (\st ps -> st { redKings   = ps })
updateBlackKings  = updatePieceset blackKings (\st ps -> st { blackKings = ps })
updateRedPieces   = updatePieceset redPieces  (\st ps -> st { redPieces  = ps })
updateBlackPieces = updatePieceset blackPieces(\st ps -> st { blackPieces= ps })

-- switches the current player turn
switchPlayer :: GameState -> GameState
switchPlayer st = st { status = if current_RedPlayer st then BlackPlayer else RedPlayer }

-- clears the message field, is called after 
-- each turn to clear the game state message
deleteMessage :: GameState -> GameState
deleteMessage st = st { message = "" }

-- returns true if the piece is a pawn
isPawn :: PorK Coord -> Bool
isPawn piece = case piece of
    P _ -> True
    _   -> False

-- returns true if the piece is a King
isKing :: PorK Coord -> Bool
isKing piece = case piece of
    K _ -> True
    _   -> False

-- add the most recent move to the game's history
appendHistory :: Move -> GameState -> GameState
appendHistory m st = st { history = m : history st }





-- main functions

-- this is the main function to be exported from this file.
-- it takes a move as input, validates it's legality
-- and then either applies it or returns am illegal message
apply_move :: Move -> GameState -> GameState
apply_move mv st = case _lookup mv (moves st) of
    EndM -> st { message = "Game Over!", status = GameOver }
    JM (start : (next : rest)) -> deleteMessage $ make_jump_move (start : (next : rest)) st
    SM [start, end] -> deleteMessage $ make_simple_move [start, end] st
    _ -> st { message = illegalMoveMessage st }


-- searches for a move in the given list of moves passed to it
-- accepts both a move to validate and a list of moves
_lookup :: Move -> SMorJM [Move] -> SMorJM Move
_lookup move moveset = case moveset of
    SM moves -> maybe (SM []) SM (search move moves)
    JM moves -> maybe (JM []) JM (search move moves)
    EndM -> EndM
  where
    search :: Eq a => a -> [a] -> Maybe a
    search _ [] = Nothing
    search x (y:ys)
        | x == y    = Just y
        | otherwise = search x ys

-- this generates the necessary message if a move is illegal per the tests in the auto grader
-- I only saw a test for when there are simple moves, but added a message for jump moves anyways.
illegalMoveMessage :: GameState -> String
illegalMoveMessage st =
    case moves st of
        JM jumps -> "Illegal move! There are jump moves:  " ++ show jumps
        SM simpleMoves -> "Illegal move! There are simple moves:  " ++ show simpleMoves

-- applies a simple move, there are two subfunctions which process both Kings and Pawns
-- on the board. The definition follows from the assignment spec.
-- for kings, it just moves the piece to the destination and switches turns.
-- for pawns, it may promote to king if the destination is on the back rank,
-- or simply move the pawn forward.
make_simple_move :: Move -> GameState -> GameState
-- case 1: moving a king piece to end position
make_simple_move [K (x, y), end] st = deleteMessage $ switchPlayer $ appendHistory [K (x, y), end] $ processKings st
  where
    -- updates the board by replacing the king's old position with the new one
    -- handles red and black moves separately
    processKings | current_RedPlayer st   = updateRedKings (replace (K (x, y)) end)
                | current_BlackPlayer st = updateBlackKings (replace (K (x, y)) end)
-- case 2: moving a pawn to 'end' position, promote if end of board
make_simple_move [P (x, y), end] st = deleteMessage $ switchPlayer $ appendHistory [P (x, y), end] $ processPieces st
  where
    -- decides how to update the piece sets depending on
    -- the current player and whether the move results in promotion
    processPieces =
        -- red player, pawn reaches ends, is removed and replaced by King
        if current_RedPlayer st && isKing end
            then updateRedKings (insertCoord end) >>> updateRedPieces (remove (P (x, y)))
        -- black player, pawn reaches ends, is removed and replaced by King
        else if current_BlackPlayer st && isKing end
            then updateBlackKings (insertCoord end) >>> updateBlackPieces (remove (P (x, y)))
        --regular red pawn move
        else if current_RedPlayer st
            then updateRedPieces (replace (P (x, y)) end)
        -- regular black pawn move
        else
            updateBlackPieces (replace (P (x, y)) end)
      where
        (>>>) = (.)


-- applies a jump move, there are two subfunctions which process both Kings and Pawns
-- on the board. The definition follows from the assignment spec. 
-- If multiple jumps, iteratively applies each jump segment and updates the board, captured pieces, and promotions.
make_jump_move :: Move -> GameState -> GameState
make_jump_move mv st = update mv (appendHistory mv st)
  where
    -- processes each jump segment recursively
    -- If there are more jumps (rest is not empty), continue jumping
    -- Otherwise, apply the last jump and switch the player
    update (start : landing : rest) st'
        | null rest = switchPlayer $ applyJump st'
        | otherwise = update (landing : rest) (applyJump st')
      where
        -- applies a single jump update on the board
        applyJump = deleteMessage . processKings . updateBoard . removeJumped

        -- handles king promotions during jumps
        processKings = case (status st, isPawn start, isKing landing) of
            (RedPlayer, True, True) ->
                updateRedKings (insertCoord landing) . updateRedPieces (remove (P (unpack_coord start)))
            (BlackPlayer, True, True) ->
                updateBlackKings (insertCoord landing) . updateBlackPieces (remove (P (unpack_coord start)))
            _ -> id

        -- removes a captured oppponent piece
        removeJumped = case status st of
            RedPlayer ->
                updateBlackKings (remove (P jumpedCoord)) . updateBlackPieces (remove (P jumpedCoord))
            BlackPlayer ->
                updateRedKings (remove (P jumpedCoord)) . updateRedPieces (remove (P jumpedCoord))
            _ -> id

        jumpedCoord = capturedSquare start landing

        -- move the piece to a new location on the board
        -- replaces old position and adds new one in the 
        -- kings or pieces list accordingly
        updateBoard = case (status st, landing, start) of
            (RedPlayer, l, s) | isPawn l  -> updateRedPieces (replace s l)
            (BlackPlayer, l, s) | isPawn l -> updateBlackPieces (replace s l)
            (RedPlayer, _, s) | isKing s  -> updateRedKings (replace s landing)
            (BlackPlayer, _, s) | isKing s -> updateBlackKings (replace s landing)
            _ -> id -- no change

