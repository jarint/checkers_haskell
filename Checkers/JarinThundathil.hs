module Checkers.JarinThundathil(moves, apply_move, red_ai, black_ai) where

import Checkers.Types
import Data.List


-- Moves.hs

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






  -- ApplyMove.hs
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


-- AI

-- AI decision functions

-- Black AI: uses minimax with alpha-beta pruning to choose the best move for Black
black_ai :: GameState -> Move
black_ai g = case snd (abmaxprune g (bot, top) 6 BlackPlayer) of
    Just m  -> m
    Nothing -> []

-- Red AI: uses minimax with alpha-beta pruning to choose the best move for Red
red_ai :: GameState -> Move
red_ai g = case snd (abmaxprune g (bot, top) 6 RedPlayer) of
    Just m  -> m
    Nothing -> []

abmaxprune :: GameState -> (Float, Float) -> Int -> Status -> (Float, Maybe Move)
abmaxprune g (alpha, beta) depth player
    | status g == GameOver = (heuristic g player, Nothing)
    | depth <= 0 && noMoreJumps g = (heuristic g player, Nothing)
    | otherwise = abmaxLoop (abmoves g) (alpha, Nothing)
  where
    abmaxLoop [] (a, mv) = (a, mv)
    abmaxLoop (m:ms) (a, mv)
      | score >= beta = (score, Just m) -- prune
      | score > a     = abmaxLoop ms (score, Just m)
      | otherwise     = abmaxLoop ms (a, mv)
      where
        score = fst (abminprune (apply_move m g) (a, beta) (depth - 1) player)

abminprune :: GameState -> (Float, Float) -> Int -> Status -> (Float, Maybe Move)
abminprune g (alpha, beta) depth player
    | status g == GameOver = (heuristic g player, Nothing)
    | depth <= 0 && noMoreJumps g = (heuristic g player, Nothing)
    | otherwise = abminLoop (abmoves g) (beta, Nothing)
  where
    abminLoop [] (b, mv) = (b, mv)
    abminLoop (m:ms) (b, mv)
      | score <= alpha = (score, Just m) -- prune
      | score < b      = abminLoop ms (score, Just m)
      | otherwise      = abminLoop ms (b, mv)
      where
        score = fst (abmaxprune (apply_move m g) (alpha, b) (depth - 1) player)

-- Select either jump moves or simple moves (prefer jumps)
abmoves :: GameState -> [Move]
abmoves g = case moves g of
    JM jumps -> jumps
    SM sims  -> sims
    EndM     -> []

-- Heuristic: simple material difference (each king worth 2, pawn worth 1)
heuristic :: GameState -> Status -> Float
heuristic g player = fromIntegral $ material player g - material (otherPlayer player) g

material :: Status -> GameState -> Int
material BlackPlayer g = length (blackPieces g) + 2 * length (blackKings g)
material RedPlayer g   = length (redPieces g) + 2 * length (redKings g)
material GameOver _    = 0

otherPlayer :: Status -> Status
otherPlayer BlackPlayer = RedPlayer
otherPlayer RedPlayer   = BlackPlayer
otherPlayer GameOver    = GameOver

-- Check if there are no jumps left
noMoreJumps :: GameState -> Bool
noMoreJumps g = case moves g of
    JM [] -> True
    JM _  -> False
    _     -> True

-- Alpha-beta bounds
top :: Float
top = 1000000

bot :: Float
bot = -1000000
