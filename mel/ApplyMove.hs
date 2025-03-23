{-# HLINT ignore "Use camelCase" #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Checkers.ApplyMove where

import           Checkers.Moves
import           Checkers.Types
------------------------------------------------
------------------------------------------------
--
--  APPLYING A MOVE to a state
--
------------------------------------------------
------------------------------------------------

apply_move :: Move -> GameState -> GameState
apply_move mv st = case findMove mv (moves st) of
    EndM -> st { message = "Game Over!", status = GameOver }
    JM (start : (next : rest)) -> makeJumpMove (start : (next : rest)) st
    SM [start, end]            -> makeSimpleMove [start, end] st
    _                          -> st { message = "Illegal Move!" }

findMove :: Move -> SMorJM [Move] -> SMorJM Move
findMove move (SM moves) = maybe (SM []) SM (find move moves)
findMove move (JM moves) = maybe (JM []) JM (find move moves)
findMove move EndM       = EndM

makeSimpleMove :: Move -> GameState -> GameState
makeSimpleMove [K (x, y), end] st = updateState st
  where
    updateState = clearMessage . changePlayer . updateHistory . updatePieces
    updatePieces | isRedPlayer st   = changeRedKings replaceEnd
                 | isBlackPlayer st = changeBlackKings replaceEnd
    replaceEnd    = replace (K (x, y)) end
    updateHistory = addToHistory [K (x, y), end]


makeSimpleMove [P (x, y), end] st = updateState st  where
    updateState = clearMessage . changePlayer . updatePieces . updateHistory
    updatePieces
        | isRedPlayer st && isKing end = changeRedKings addEnd
        . changeRedPieces removeStart
        | isBlackPlayer st && isKing end = changeBlackKings addEnd
        . changeBlackPieces removeStart
        | isRedPlayer st = changeRedPieces replaceEnd
        | isBlackPlayer st = changeBlackPieces replaceEnd
    addEnd        = add end
    removeStart   = remove (P (x, y))
    replaceEnd    = replace (P (x, y)) end
    updateHistory = addToHistory [P (x, y), end]



makeJumpMove :: Move -> GameState -> GameState
makeJumpMove mv st = update mv (addToHistory mv st)
  where
    update (start : landing : rest) st
        | null rest = (changePlayer . updateState) st
        | otherwise = update (landing : rest) (updateState st)
      where
        updateState :: GameState -> GameState
        updateState =
            clearMessage . updateKings . updatePlacements . removeJumpedPiece
        -- if a pawn become a king, we need to update kings
        updateKings :: GameState -> GameState
        updateKings
            | isRedPlayer st && isPawn start && isKing landing
            = changeRedKings (add landing) . changeRedPieces (remove start)
            | isBlackPlayer st && isPawn start && isKing landing
            = changeBlackKings (add landing) . changeBlackPieces (remove start)
            | otherwise
            = id
        removeJumpedPiece
            | isRedPlayer st = changeBlackKings removeJumped
            . changeBlackPieces removeJumped
            | isBlackPlayer st = changeRedKings removeJumped
            . changeRedPieces removeJumped
            | otherwise = id
        removeJumped = remove (jumped start landing)
        updatePlacements
            | isRedPlayer st && isPawn landing
            = changeRedPieces replaceLanding
            | isBlackPlayer st && isPawn landing
            = changeBlackPieces replaceLanding
            | isRedPlayer st && isKing start
            = changeRedKings replaceLanding
            | isBlackPlayer st && isKing start
            = changeBlackKings replaceLanding
            | otherwise
            = id
        replaceLanding = replace start landing



addToHistory :: Move -> GameState -> GameState
addToHistory m st = st { history = m : history st }

remove :: PorK Coord -> PieceState -> PieceState
remove piece = filter (/= coord piece)

add :: PorK Coord -> PieceState -> PieceState
add piece = (:) (coord piece)

jumped :: PorK Coord -> PorK Coord -> PorK Coord
jumped c1 c2 = P (x + 1 * dirX, y + 1 * dirY)  where
    dirX     = (x' - x) `div` abs (x' - x)
    dirY     = (y' - y) `div` abs (y' - y)
    (x , y ) = coord c1
    (x', y') = coord c2



replace :: PorK Coord -> PorK Coord -> PieceState -> PieceState
replace coord1 coord2 = (:) (coord coord2) . filter (/= coord coord1)

changeRedKings :: (PieceState -> PieceState) -> GameState -> GameState
changeRedKings operation st = st { redKings = operation (redKings st) }

changeBlackKings :: (PieceState -> PieceState) -> GameState -> GameState
changeBlackKings operation st = st { blackKings = operation (blackKings st) }

changeRedPieces :: (PieceState -> PieceState) -> GameState -> GameState
changeRedPieces operation st = st { redPieces = operation (redPieces st) }

changeBlackPieces :: (PieceState -> PieceState) -> GameState -> GameState
changeBlackPieces operation st =
    st { blackPieces = operation (blackPieces st) }

changePlayer :: GameState -> GameState
changePlayer st = st { status = nextPlayer }  where
    nextPlayer :: Status
    nextPlayer | isRedPlayer st = BlackPlayer
               | otherwise      = RedPlayer

clearMessage :: GameState -> GameState
clearMessage st = st { message = "" }


find :: (Eq a) => a -> [a] -> Maybe a
find x [] = Nothing
find x (y : ys) | x == y    = Just x
                | otherwise = find x ys

isPawn :: PorK Coord -> Bool
isPawn (P (x, y)) = True
isPawn _          = False
