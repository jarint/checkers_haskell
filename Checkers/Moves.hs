module Checkers.Moves where

import Checkers.Types
import Data.List

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
jump_over :: [Move] -> [Move]
jump_over [] = [[]]
jump_over z = z

-- filterLongestMoves :: [Move] -> [Move]
-- filterLongestMoves moves
--     | null moves = []
--     | otherwise = let maxLength = maximum (map length moves)
--                   in filter ((== maxLength) . length) moves

filtersubmoves :: [Move] -> [Move]
filtersubmoves [] = []
filtersubmoves (x:xs)
    | isSubmove x xs = filtersubmoves xs
    | otherwise = x : filtersubmoves xs
  where
    isSubmove :: Move -> [Move] -> Bool
    isSubmove a [] = False
    isSubmove a (b:bs)
        | a `isPrefixOf` b = True
        | otherwise = isSubmove a bs

isInBoard :: Coord -> Bool
isInBoard (x, y) = x >= 0 && x < 8 && y >= 0 && y < 8

isOccupied :: Coord -> GameState -> Bool
isOccupied c st = c `elem` (blackPieces st ++ redPieces st ++ blackKings st ++ redKings st)

isOpponentSquare :: Coord -> GameState -> Bool
isOpponentSquare c st = case status st of
    RedPlayer -> c `elem` (blackPieces st ++ blackKings st)
    BlackPlayer -> c `elem` (redPieces st ++ redKings st)
    _ -> False

-- repeated states
updateMoveHistory :: [Coord] -> [Coord]
updateMoveHistory [] = []
updateMoveHistory (x:xs)
    | x `elem` xs = updateMoveHistory (filter (/= x) xs)
    | otherwise = x : updateMoveHistory xs

detectRepeatedState :: Move -> [Move] -> Bool
detectRepeatedState mv historyMoves =
    let updateList lst coord = if coord `elem` lst then filter (/= coord) lst else coord : lst
        finalList = foldl (\lst move -> foldl updateList lst [coord | K coord <- move]) [coord | K coord <- mv] historyMoves
    in null finalList

-- Move Generation
moves :: GameState -> (SMorJM [Move])
moves st
    | not (null jumps) = JM jumps
    | not (null simples) = SM simples
    | otherwise = EndM
  where
    jumps = jump_moves st
    simples = filter (not . (`detectRepeatedState` history st)) (simple_moves st)


simple_moves :: GameState -> [Move]
simple_moves st
    | status st == RedPlayer = simpleKing (redKings st) st ++ simplePiece (redPieces st) 1 st
    | status st == BlackPlayer = simpleKing (blackKings st) st ++ simplePiece (blackPieces st) (-1) st
    | otherwise = []

simplePiece :: [Coord] -> Int -> GameState -> [Move]
simplePiece xs dir st =
    [ [P (x, y), if ny == 0 || ny == 7 then K (nx, ny) else P (nx, ny)]
    | (x, y) <- xs
    , (nx, ny) <- [(x + 1, y + dir), (x - 1, y + dir)]
    , isInBoard (nx, ny) && not (isOccupied (nx, ny) st)]

simpleKing :: [Coord] -> GameState -> [Move]
simpleKing xs st =
    [ [K (x, y), K (nx, ny)]
    | (x, y) <- xs
    , (nx, ny) <- [(x + 1, y + 1), (x - 1, y + 1), (x + 1, y - 1), (x - 1, y - 1)]
    , isInBoard (nx, ny) && not (isOccupied (nx, ny) st)]

-- jump_moves :: GameState -> [Move]
-- jump_moves st
--     | status st == RedPlayer =
--         let kingJumps = jumpKing (redKings st) st
--             pawnJumps = jumpPawn (redPieces st) st
--         in filterLongestMoves (kingJumps ++ pawnJumps)
--     | status st == BlackPlayer =
--         let kingJumps = jumpKing (blackKings st) st
--             pawnJumps = jumpPawn (blackPieces st) st
--         in filterLongestMoves (kingJumps ++ pawnJumps)
--     | otherwise = []

jump_moves :: GameState -> [Move]
jump_moves st
    | status st == RedPlayer = filtersubmoves (reverse (filtersubmoves (reverse (jumpKing (redKings st) st ++ jumpPawn (redPieces st) st))))
    | status st == BlackPlayer = filtersubmoves (reverse (filtersubmoves (reverse (jumpKing (blackKings st) st ++ jumpPawn (blackPieces st) st))))
    | otherwise = []


jumpKing :: [Coord] -> GameState -> [Move]
jumpKing xs st = [ [K (x, y)] ++ path | (x, y) <- xs, path <- jumpKingHelper (x, y) [] (x, y) st]

--jumpKingHelper :: Coord -> [Coord] -> Coord -> GameState -> [Move]
--jumpKingHelper start rem (x, y) st =
--    [ [K (x'', y'')] ++ ys
--    | ((x', y'), (x'', y'')) <- [((x + 1, y + 1), (x + 2, y + 2)), 
--                                 ((x - 1, y + 1), (x - 2, y + 2)), 
--                                 ((x + 1, y - 1), (x + 2, y - 2)), 
--                                 ((x - 1, y - 1), (x - 2, y - 2))]
--    , notElem (x', y') rem
--    , isOpponentSquare (x', y') st
--    , isInBoard (x'', y'') 
--    , not (isOccupied (x'', y'') st)
--    , ys <- jumpKingHelper start ((x', y') : rem) (x'', y'') st ++ [[]]
--    ]

jumpKingHelper :: Coord -> [Coord] -> Coord -> GameState -> [Move]
jumpKingHelper start visited (x, y) st =
    [ [K (ex, ey)] ++ rest
    | ((cx, cy), (ex, ey)) <- [((x+1, y+1), (x+2, y+2)), ((x-1, y+1), (x-2, y+2)), ((x+1, y-1), (x+2, y-2)), ((x-1, y-1), (x-2, y-2))]
    , not ((cx, cy) `elem` visited)
    , isOpponentSquare (cx, cy) st
    , (start == (ex, ey)) || (isInBoard (ex, ey) && not (isOccupied (ex, ey) st))
    , rest <- jump_over (jumpKingHelper start ((cx, cy) : visited) (ex, ey) st)]

jumpPawn :: [Coord] -> GameState -> [Move]
jumpPawn xs st = [ [P (x, y)] ++ path | (x, y) <- xs, path <- jumpPawnHelper (x, y) [] (x, y) st]

-- jumpPawnHelper :: Coord -> [Coord] -> Coord -> GameState -> [Move]
-- jumpPawnHelper start rem (x, y) st =
--     [ [P (x'', y'')] ++ ys
--     | ((x', y'), (x'', y'')) <- [((x + 1, y + dir), (x + 2, y + 2 * dir)), 
--                                  ((x - 1, y + dir), (x - 2, y + 2 * dir))]
--     , notElem (x', y') rem
--     , isOpponentSquare (x', y') st
--     , isInBoard (x'', y'') 
--     , not (isOccupied (x'', y'') st)
--     , ys <- jumpPawnHelper start ((x', y') : rem) (x'', y'') st ++ [[]]
--     ]
--   where
--     dir = if status st == RedPlayer then 1 else -1

jumpPawnHelper :: Coord -> [Coord] -> Coord -> GameState -> [Move]
jumpPawnHelper start visited (x, y) st =
    [ (if ny == 0 || ny == 7 then K (ex, ny) else P (ex, ny)) : rest
    | ((cx, cy), (ex, ny)) <- if status st == RedPlayer then [((x+1, y+1), (x+2, y+2)), ((x-1, y+1), (x-2, y+2))] else [((x+1, y-1), (x+2, y-2)), ((x-1, y-1), (x-2, y-2))]
    , not ((cx, cy) `elem` visited)
    , isOpponentSquare (cx, cy) st
    , (start == (ex, ny)) || (isInBoard (ex, ny) && not (isOccupied (ex, ny) st))
    , rest <- jump_over (if ny == 0 || ny == 7 then jumpKingHelper start ((cx, cy):visited) (ex, ny) st else jumpPawnHelper start ((cx, cy):visited) (ex, ny) st)]


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
