module Checkers.Moves where

import Checkers.Types
import Data.List


-- Utilities

-- direction: returns movement direction depending on the current player
-- Red pieces move "up" the board (-1), black pieces move "down" the board (+1)
direction :: GameState -> Int
direction g = if status g == RedPlayer then -1 else 1

-- jumpOver: ensures recursion continues for jumps
-- returns [[]] if no moves are found, allowing backtracking
jumpOver :: [[a]] -> [[a]]
jumpOver xs
  | null xs   = [[]]
  | otherwise = xs

-- checks if a coordinate is within the board boundaries
isInBoard :: Coord -> Bool
isInBoard (x, y) = x >= 0 && x <= 7 && y >= 0 && y <= 7

-- checks if a given coordinate is not occupied by any piece
isFreeSquare :: Coord -> GameState -> Bool
isFreeSquare c g = not (occupied c (redPieces g ++ redKings g ++ blackPieces g ++ blackKings g))

-- checks if the given coordinate has an opponent's piece
isOpponentSquare :: Coord -> GameState -> Bool
isOpponentSquare c g = c `elem` (if status g == RedPlayer then blackPieces g ++ blackKings g else redPieces g ++ redKings g)

-- helper function to check if a coordinate exists in a list of coordinates
occupied :: Coord -> [Coord] -> Bool
occupied c coords = c `elem` coords

-- unwraps a PorK Coord to just a Coord
getCoord :: PorK Coord -> Coord
getCoord (P c) = c
getCoord (K c) = c

-- checks if a move starts with a king
isMoveByKing :: Move -> Bool
isMoveByKing (K _ : _) = True
isMoveByKing _         = False


-- Move Generation

-- required to export
-- moves: generates either jump moves or simple moves, preferring jumps if they exist
moves :: GameState -> SMorJM [Move]
moves st
  | not (null jumpMovesInSt)   = JM jumpMovesInSt
  | not (null simpleMovesInSt) = SM simpleMovesInSt
  | otherwise                  = EndM
  where
    simpleMovesInSt = simpleMoves st
    jumpMovesInSt   = jumpMoves st


-- HISTORY TRACKING

-- prevents infinite cycles by checking king move repetitions
noCycle :: Move -> [Move] -> Bool
noCycle = detectMoveCycle []

-- recursively checks if a series of king moves leads to a cycle
detectMoveCycle :: [PorK Coord] -> [PorK Coord] -> [Move] -> Bool
detectMoveCycle [] [] _  = False
detectMoveCycle _  _  [] = True
detectMoveCycle prevSet currSet (m:ms) =
  typeIsJumpMove m || not (isMoveByKing m) || detectMoveCycle currSet (updateCycleTracker m prevSet) ms

-- updates cycle tracking sets for king moves
updateCycleTracker :: Move -> [PorK Coord] -> [PorK Coord]
updateCycleTracker [start, end] = flipPiecePosition end . flipPiecePosition start
updateCycleTracker _ = id

-- flips on or off a piece's presence in the cycle tracker list
flipPiecePosition :: PorK Coord -> [PorK Coord] -> [PorK Coord]
flipPiecePosition x [] = [x]
flipPiecePosition x (y:ys)
  | x == y    = ys
  | otherwise = y : flipPiecePosition x ys






-- SIMPLE MOVES

-- generates all simple (non-jump) moves for the current player
simpleMoves :: GameState -> [Move]
simpleMoves st = case status st of
  RedPlayer   -> simpleKing (redKings st) st (history st) ++ simplePiece (redPieces st) (allBlackPieces st) st
  BlackPlayer -> simpleKing (blackKings st) st (history st) ++ simplePiece (blackPieces st) (allRedPieces st) st
  _           -> []
  where
    allBlackPieces = (++ blackPieces st) . blackKings
    allRedPieces = (++ redPieces st) . redKings

-- Generates all valid simple (non-jump) moves for pawns.
-- For each pawn:
--   - Calculates its forward direction based on the current player (using `direction`).
--   - Considers both possible diagonal moves forward: (x+1, y+dy) and (x-1, y+dy).
--   - Checks that each target square is both on the board and not occupied by an opponent piece.
--   - If a pawn reaches the last rank (y' == 0 for Red, y' == 7 for Black), it is promoted to a king.
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

-- Generates all valid simple (non-jump) moves for kings.
-- For each king on the board:
--   - Considers all four diagonal moves (since kings move both forward and backward).
--   - Checks that the target square (x', y') is on the board and unoccupied.
--   - Uses noCycle to ensure that the move does not repeat a previous position.
-- Each move is represented as a two-element list [start, end].
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

-- determines if a move is a jump by checking displacement
typeIsJumpMove :: Move -> Bool
typeIsJumpMove [start, end] = abs (fst (getCoord start) - fst (getCoord end)) /= 1
typeIsJumpMove _           = True

-- generates all possible jump moves for the current player
jumpMoves :: GameState -> [Move]
jumpMoves st = case status st of
  RedPlayer   -> jumpKing (redKings st) st ++ jumpPawn (redPieces st) st
  BlackPlayer -> jumpKing (blackKings st) st ++ jumpPawn (blackPieces st) st
  _           -> []

-- jumpKing: generates jump moves for king pieces, chaining through jumpKingHelper
jumpKing :: [Coord] -> GameState -> [Move]
jumpKing coords st =
  [ K (x, y) : rest
  | (x, y) <- coords
  , rest <- jumpKingHelper (x, y) [] (x, y) st
  ]

-- Recursively finds all possible chained jump sequences for kings.
-- Starting from the current king position (x, y), it looks for jumps in all
-- four diagonal directions.
-- For each direction:
--   - (cx, cy) is the square occupied by an opponent piece being jumped over.
--   - (x'', y'') is the landing square two steps beyond that piece.
--   - continue jumping when:
--       1. The opponent piece hasn't already been jumped over in this chain.
--       2. The landing square is on the board and free, or is the starting square.
-- After finding a valid jump, it recursively calls jumpKingHelper again to continue chaining.
-- The recursion builds up complete sequences of jump moves for the king, with each jump step prepended.
jumpKingHelper :: Coord -> [Coord] -> Coord -> GameState -> [Move]
jumpKingHelper start visited (x, y) st =
  [ K (x'', y'') : rest
  | ((cx, cy), (x'', y'')) <- [((x + 1, y + 1), (x + 2, y + 2)), ((x - 1, y + 1), (x - 2, y + 2)), ((x + 1, y - 1), (x + 2, y - 2)), ((x - 1, y - 1), (x - 2, y - 2))]
  , not (elem (cx, cy) visited), isOpponentSquare (cx, cy) st
  , isInBoard (x'', y'')
  , isFreeSquare (x'', y'') st || (x'', y'') == start
  , rest <- jumpOver (jumpKingHelper start ((cx, cy):visited) (x'', y'') st)
  ]

-- generates jump moves for pawns, calling jumpPawnHelper for chains
jumpPawn :: [Coord] -> GameState -> [Move]
jumpPawn coords st =
  [ P (x, y) : landingMoves
  | (x, y) <- coords
  , landingMoves <- jumpPawnHelper (x, y) [] (x, y) st
  ]

-- Recursively finds all possible chained jump sequences for pawn pieces.
-- The function starts from a given coordinate and explores possible jumps
-- in both diagonal directions (left and right), skipping over opponent pieces.
-- 'visited' keeps track of already jumped-over coordinates to avoid loops.
-- For each valid jump:
--   - (cx, cy) is the opponent’s piece being jumped over.
--   - (x'', y'') is the landing square two steps beyond that opponent.
--   - The move is only valid if the landing square is on the board and free.
-- After performing a jump, the function either:
--   1. Continues recursively as jumpPawnHelper if the piece is still a pawn and not on the end row.
--   2. Switches to jumpKingHelper if the piece just reached the last rank (promotion row),
--     meaning it became a king and can chain jumps with king move logic.
-- The pieceType function at the end determines if the current jump should promote the pawn to a king
-- by checking if it landed on the opponent’s back row.
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
