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
unpack_coord :: PorK Coord -> Coord
unpack_coord (P c) = c
unpack_coord (K c) = c

current_RedPlayer :: GameState -> Bool
current_RedPlayer st = status st == RedPlayer
current_BlackPlayer :: GameState -> Bool
current_BlackPlayer st = status st == BlackPlayer

remove :: PorK Coord -> PieceState -> PieceState
remove piece = filter (/= unpack_coord piece)

replace :: PorK Coord -> PorK Coord -> PieceState -> PieceState
replace from to = (:) (unpack_coord to) . filter (/= unpack_coord from)

insertCoord :: PorK Coord -> PieceState -> PieceState
insertCoord piece = (:) (unpack_coord piece)

capturedSquare :: PorK Coord -> PorK Coord -> Coord
capturedSquare c1 c2 = (startX + stepX, startY + stepY)
  where
    (startX, startY) = unpack_coord c1
    (endX, endY)     = unpack_coord c2
    stepX            = signum (endX - startX)
    stepY            = signum (endY - startY)


updatePieceset :: (GameState -> PieceState) -> (GameState -> PieceState -> GameState) -> (PieceState -> PieceState) -> GameState -> GameState
updatePieceset getter setter op st = setter st (op (getter st))
updateRedKings    = updatePieceset redKings   (\st ps -> st { redKings   = ps })
updateBlackKings  = updatePieceset blackKings (\st ps -> st { blackKings = ps })
updateRedPieces   = updatePieceset redPieces  (\st ps -> st { redPieces  = ps })
updateBlackPieces = updatePieceset blackPieces(\st ps -> st { blackPieces= ps })

switchPlayer :: GameState -> GameState
switchPlayer st = st { status = if current_RedPlayer st then BlackPlayer else RedPlayer }

deleteMessage :: GameState -> GameState
deleteMessage st = st { message = "" }

isPawn :: PorK Coord -> Bool
isPawn piece = case piece of
    P _ -> True
    _   -> False

isKing :: PorK Coord -> Bool
isKing piece = case piece of
    K _ -> True
    _   -> False


appendHistory :: Move -> GameState -> GameState
appendHistory m st = st { history = m : history st }


-- main functions
apply_move :: Move -> GameState -> GameState
apply_move mv st = case _lookup mv (moves st) of
    EndM -> st { message = "Game Over!", status = GameOver }
    JM (start : (next : rest)) -> deleteMessage $ make_jump_move (start : (next : rest)) st
    SM [start, end] -> deleteMessage $ make_simple_move [start, end] st
    _ -> st { message = illegalMoveMessage st }


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


illegalMoveMessage :: GameState -> String
illegalMoveMessage st =
    case moves st of
        JM jumps -> "Illegal move! There are jump moves:  " ++ show jumps
        SM simpleMoves -> "Illegal move! There are simple moves:  " ++ show simpleMoves


make_simple_move :: Move -> GameState -> GameState
make_simple_move [K (x, y), end] st = deleteMessage $ switchPlayer $ appendHistory [K (x, y), end] $ processKings st
  where
    processKings | current_RedPlayer st   = updateRedKings (replace (K (x, y)) end)
                | current_BlackPlayer st = updateBlackKings (replace (K (x, y)) end)

make_simple_move [P (x, y), end] st = deleteMessage $ switchPlayer $ appendHistory [P (x, y), end] $ processPieces st
  where
    processPieces =
        if current_RedPlayer st && isKing end
            then updateRedKings (insertCoord end) >>> updateRedPieces (remove (P (x, y)))
        else if current_BlackPlayer st && isKing end
            then updateBlackKings (insertCoord end) >>> updateBlackPieces (remove (P (x, y)))
        else if current_RedPlayer st
            then updateRedPieces (replace (P (x, y)) end)
        else
            updateBlackPieces (replace (P (x, y)) end)
      where
        (>>>) = (.)



make_jump_move :: Move -> GameState -> GameState
make_jump_move mv st = update mv (appendHistory mv st)
  where
    update (start : landing : rest) st'
        | null rest = switchPlayer $ applyJump st'
        | otherwise = update (landing : rest) (applyJump st')
      where
        applyJump = deleteMessage . processKings . updateBoard . removeJumped

        processKings = case (status st, isPawn start, isKing landing) of
            (RedPlayer, True, True) ->
                updateRedKings (insertCoord landing) . updateRedPieces (remove (P (unpack_coord start)))
            (BlackPlayer, True, True) ->
                updateBlackKings (insertCoord landing) . updateBlackPieces (remove (P (unpack_coord start)))
            _ -> id

        removeJumped = case status st of
            RedPlayer ->
                updateBlackKings (remove (P jumpedCoord)) . updateBlackPieces (remove (P jumpedCoord))
            BlackPlayer ->
                updateRedKings (remove (P jumpedCoord)) . updateRedPieces (remove (P jumpedCoord))
            _ -> id

        jumpedCoord = capturedSquare start landing

        updateBoard = case (status st, landing, start) of
            (RedPlayer, l, s) | isPawn l  -> updateRedPieces (replace s l)
            (BlackPlayer, l, s) | isPawn l -> updateBlackPieces (replace s l)
            (RedPlayer, _, s) | isKing s  -> updateRedKings (replace s landing)
            (BlackPlayer, _, s) | isKing s -> updateBlackKings (replace s landing)
            _ -> id
