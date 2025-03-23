module Checkers.Moves where

import           Checkers.Types

moves :: GameState -> SMorJM [Move]
moves st | thereAre jumpMovesInSt   = JM jumpMovesInSt
         | thereAre simpleMovesInSt = SM simpleMovesInSt
         | otherwise                = EndM
  where
    simpleMovesInSt = simpleMoves stat rks rps bks bps kORp dir hist
    jumpMovesInSt   = jumpMoves stat rks rps bks bps kORp dir
    thereAre        = not . null
    dir             = direction st
    kORp            = kingOrPawn st
    bks             = blackKings st
    rks             = redKings st
    bps             = blackPieces st
    rps             = redPieces st
    hist            = history st
    stat            = status st




simpleMoves
    :: Status
    -> PieceState
    -> PieceState
    -> PieceState
    -> PieceState
    -> (Coord -> PorK Coord)
    -> Int
    -> [Move]
    -> [Move]
simpleMoves stat rks rps bks bps kORp dir hist = case stat of
    RedPlayer   -> redKingsSimpleMoves ++ redPiecesSimpleMoves
    BlackPlayer -> blackKingsSimpleMoves ++ blackPiecesSimpleMoves
    _           -> []
  where
    redKingsSimpleMoves    = simpleKing rks (rps ++ bs) hist
    redPiecesSimpleMoves   = simplePiece rps (rks ++ bs) kORp dir
    blackKingsSimpleMoves  = simpleKing bks (bps ++ rs) hist
    blackPiecesSimpleMoves = simplePiece bps (bks ++ rs) kORp dir
    bs                     = bks ++ bps
    rs                     = rks ++ rps


jumpMoves
    :: Status
    -> PieceState
    -> PieceState
    -> PieceState
    -> PieceState
    -> (Coord -> PorK Coord)
    -> Int
    -> [Move]
jumpMoves stat rks rps bks bps kORp dir = case stat of
    RedPlayer   -> redKingsJumpMoves ++ redPiecesJumpMoves
    BlackPlayer -> blackKingsJumpMoves ++ blackPiecesJumpMoves
    _           -> []
  where
    redKingsJumpMoves    = jumpKing rks rps bs
    redPiecesJumpMoves   = jumpPiece rps rks bs kORp dir
    blackKingsJumpMoves  = jumpKing bks bps rs
    blackPiecesJumpMoves = jumpPiece bps bks rs kORp dir
    bs                   = bks ++ bps
    rs                   = rks ++ rps




simplePiece
    :: PieceState -> PieceState -> (Coord -> PorK Coord) -> Int -> [Move]
simplePiece pieces others kORp dir =
    [ [P (x, y), kORp (x', y')]
    | (x , y ) <- pieces
    , (x', y') <- [(x + 1, y + dir), (x - 1, y + dir)]
    , not (occupied (x', y') (pieces ++ others)) && onBoard (x', y')
    ]


simpleKing :: PieceState -> PieceState -> [Move] -> [Move]
simpleKing kings others history =
    [ [K (x, y), K (x', y')]
    | (x , y ) <- kings
    , (x', y') <-
        [(x + 1, y + 1), (x - 1, y + 1), (x + 1, y - 1), (x - 1, y - 1)]
    , not (occupied (x', y') (kings ++ others))
        && onBoard (x', y')
        && notRepeated [K (x, y), K (x', y')] history
    ]


jumpPiece
    :: PieceState
    -> PieceState
    -> PieceState
    -> (Coord -> PorK Coord)
    -> Int
    -> [Move]
jumpPiece pieces kings opponents kORp dir =
    [ P (x, y) : landings
    | (x, y)   <- pieces
    , landings <- jumpPiece' (x, y) [] (x, y)
    ]
  where
    jumpPiece' start rem (x, y) =
        [ kORp (x'', y'') : landings
        | ((x', y'), (x'', y'')) <-
            [ ((x + 1, y + dir), (x + 2, y + 2 * dir))
            , ((x - 1, y + dir), (x - 2, y + 2 * dir))
            ]
        , not (occupied (x'', y'') (pieces ++ kings ++ opponents))
            && onBoard (x'', y'')
            && occupied (x', y') opponents
        , landings <- jumpOver
            (if kORp (x'', y'') == K (x'', y'')
                then map
                    tail
                    (jumpKing
                        [(x'', y'')]
                        (filter (/= start) pieces)
                        (filter (`notElem` ((x', y') : rem)) opponents)
                    )
                else jumpPiece' start ((x', y') : rem) (x'', y'')
            )
        ]

jumpKing :: PieceState -> PieceState -> PieceState -> [Move]
jumpKing kings pieces opponents =
    [ K (x, y) : landings
    | (x, y)   <- kings
    , landings <- jumpKing' (x, y) [] (x, y)
    ]
  where
    jumpKing' start rem (x, y) =
        [ K (x'', y'') : landings
        | ((x', y'), (x'', y'')) <-
            [ ((x + 1, y + 1), (x + 2, y + 2))
            , ((x - 1, y + 1), (x - 2, y + 2))
            , ((x + 1, y - 1), (x + 2, y - 2))
            , ((x - 1, y - 1), (x - 2, y - 2))
            ]
        , (x', y')
            `notElem` rem
            &&        occupied (x', y') opponents
            &&        (start == (x'', y'') || not
                          (occupied (x'', y'') (opponents ++ pieces ++ kings))
                      )
            &&        onBoard (x'', y'')
        , landings <- jumpOver (jumpKing' start ((x', y') : rem) (x'', y''))
        ]

jumpOver :: [[a]] -> [[a]]
jumpOver [] = [[]]
jumpOver z  = z



occupied :: Coord -> PieceState -> Bool
occupied coord coords = coord `elem` coords


kingOrPawn :: GameState -> Coord -> PorK Coord
kingOrPawn st (x, y)
    | ((status st == RedPlayer) && y == 0)
        || ((status st == BlackPlayer) && y == 7)
    = K (x, y)
    | otherwise
    = P (x, y)


direction :: GameState -> Int
direction st | status st == RedPlayer = -1
             | otherwise              = 1


onBoard :: Coord -> Bool
onBoard (x, y) = x >= 0 && x <= 7 && y >= 0 && y <= 7

notRepeated :: Move -> [Move] -> Bool
notRepeated = checkRepetition []

checkRepetition :: [PorK Coord] -> [PorK Coord] -> [Move] -> Bool
checkRepetition [] [] _  = False
checkRepetition _  _  [] = True
checkRepetition l1 l2 (m : ms) =
    isJumpMove m
        || not (isKingMove m)
        || checkRepetition l2 (togglePresence m l1) ms

togglePresence :: Move -> [PorK Coord] -> [PorK Coord]
togglePresence [s, e] l =
    (togglePresenceOfOneElement s . togglePresenceOfOneElement e) l
togglePresence _ l = l

togglePresenceOfOneElement :: PorK Coord -> [PorK Coord] -> [PorK Coord]
togglePresenceOfOneElement s [] = [s]
togglePresenceOfOneElement s (x : xs)
    | x == s    = xs
    | otherwise = x : togglePresenceOfOneElement s xs

isJumpMove :: Move -> Bool
isJumpMove [s, e] = abs (x - x') /= 1  where
    (x , _) = coord s
    (x', _) = coord e
isJumpMove _ = True


isKingMove :: Move -> Bool
isKingMove [s, _]  = isKing s
isKingMove (s : r) = isKing s
isKingMove _       = False

isRedPlayer :: GameState -> Bool
isRedPlayer st = status st == RedPlayer

isBlackPlayer :: GameState -> Bool
isBlackPlayer st = status st == BlackPlayer

coord :: PorK Coord -> Coord
coord (P (x, y)) = (x, y)
coord (K (x, y)) = (x, y)

isKing :: PorK Coord -> Bool
isKing (P _) = False
isKing (K _) = True


stttt = GameState { redKings    = [(1, 0), (0, 1)]
                  , redPieces   = [(3, 4)]
                  , blackKings  = [(4, 5)]
                  , blackPieces = [(2, 1), (4, 1), (2, 3), (4, 3)]
                  , status      = RedPlayer
                  , history     = []
                  , message     = ""
                  }
