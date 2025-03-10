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

remove :: Eq a => a -> [a] -> [a]
remove x = filter (/= x)

replace :: Eq a => a -> a -> [a] -> [a]
replace old new = map (\p -> if p == old then new else p)

jumped :: Coord -> Coord -> Coord
jumped (x1, y1) (x2, y2) = ((x1 + x2) `div` 2, (y1 + y2) `div` 2)

-- history tracking

updateGameHistory :: GameState -> Move -> GameState
updateGameHistory st mv
    | isKingMove mv = st { history = mv : history st }
    | otherwise = st
  where
    isKingMove ((K _):_) = True
    isKingMove _ = False

checkForRepetition :: GameState -> Bool
checkForRepetition st =
    let pastMoves = history st
        moveTracker = foldl updateTrackingList [] pastMoves
    in null moveTracker

updateTrackingList :: [Coord] -> Move -> [Coord]
updateTrackingList acc move =
    foldl (\lst coord -> if coord `elem` lst then remove coord lst else coord : lst) acc (extractKingCoords move)

extractKingCoords :: Move -> [Coord]
extractKingCoords = foldr (\(K c) acc -> c : acc) []

-- main functions

apply_move :: Move -> GameState -> GameState
apply_move mv st
    | null validMoves = st { status = GameOver, message = "!!GAME OVER!!" }
    | not (mv `elem` validMoves) = handleIllegalMove st
    | otherwise =
        let newState = applyValidMove mv st
        in if checkForRepetition newState
            then newState { status = GameOver, message = "!!REPEATED STATE - GAME OVER!!" }
            else newState
  where
    validMoves = case moves st of
        JM jumpMoves -> jumpMoves
        SM simpleMoves -> simpleMoves
        EndM -> []

handleIllegalMove :: GameState -> GameState
handleIllegalMove st =
    let validSimpleMoves = case moves st of
            SM simpleMoves -> simpleMoves
            _ -> []
        msg = if null validSimpleMoves
              then "Illegal move!"
              else "Illegal move! There are simple moves: " ++ show validSimpleMoves
    in st { message = msg }

applyValidMove :: Move -> GameState -> GameState
applyValidMove mv st
    | isJumpMove mv = applyJumpMove mv updatedState
    | otherwise = applySimpleMove mv updatedState
  where
    updatedState = updateGameHistory st mv
    isJumpMove (_:_:_) = True
    isJumpMove _ = False

applySimpleMove :: Move -> GameState -> GameState
applySimpleMove [start, end] st =
    case (start, end) of
        (P s, P e) | status st == RedPlayer && s `elem` redPieces st ->
            st { redPieces = replace s e (redPieces st)
               , status = BlackPlayer
               , message = "Black Player's turn"
               }
        (P s, P e) | status st == BlackPlayer && s `elem` blackPieces st ->
            st { blackPieces = replace s e (blackPieces st)
               , status = RedPlayer
               , message = "Red Player's turn"
               }
        (K s, K e) | status st == RedPlayer && s `elem` redKings st ->
            st { redKings = replace s e (redKings st)
               , status = BlackPlayer
               , message = "Black Player's turn"
               }
        (K s, K e) | status st == BlackPlayer && s `elem` blackKings st ->
            st { blackKings = replace s e (blackKings st)
               , status = RedPlayer
               , message = "Red Player's turn"
               }
        _ -> handleIllegalMove st

applyJumpMove :: Move -> GameState -> GameState
applyJumpMove (start:next:rest) st =
    case (start, next) of
        (P s, P n) | status st == RedPlayer && s `elem` redPieces st ->
            applyJumpMove (next:rest) (capturePiece st s n RedPlayer)
        (P s, P n) | status st == BlackPlayer && s `elem` blackPieces st ->
            applyJumpMove (next:rest) (capturePiece st s n BlackPlayer)
        (K s, K n) | status st == RedPlayer && s `elem` redKings st ->
            applyJumpMove (next:rest) (capturePiece st s n RedPlayer)
        (K s, K n) | status st == BlackPlayer && s `elem` blackKings st ->
            applyJumpMove (next:rest) (capturePiece st s n BlackPlayer)
        _ -> handleIllegalMove st
applyJumpMove [P (x, y)] st = promoteIfAvailable st (x, y)
applyJumpMove [K (x, y)] st = promoteIfAvailable st (x, y)

capturePiece :: GameState -> Coord -> Coord -> Status -> GameState
capturePiece st start end player =
    let mid = jumped start end
        newState = case player of
            RedPlayer -> st { redPieces = replace start end (redPieces st)
                            , blackPieces = remove mid (blackPieces st)
                            , blackKings = remove mid (blackKings st)
                            }
            BlackPlayer -> st { blackPieces = replace start end (blackPieces st)
                              , redPieces = remove mid (redPieces st)
                              , redKings = remove mid (redKings st)
                              }
    in promoteIfAvailable newState end

promoteIfAvailable :: GameState -> Coord -> GameState
promoteIfAvailable st (x, y)
    | y == 0 && (x, y) `elem` redPieces st =
        st { redPieces = remove (x, y) (redPieces st), redKings = (x, y) : redKings st }
    | y == 7 && (x, y) `elem` blackPieces st =
        st { blackPieces = remove (x, y) (blackPieces st), blackKings = (x, y) : blackKings st }
    | otherwise = st
