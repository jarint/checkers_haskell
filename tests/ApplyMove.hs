module ApplyMove where

import Moves
import Checkers.Types

-- This function is our "main" function to apply a move to a GameState. If our move is in simple_moves and jump_moves is empty, we call make_simple_move which
-- changes our GameState accordingly, and we also add our move to history after this function returns. If our move is in simple_moves and jump_moves is not 
-- empty, we return the same GameState but with a message suggesting a jump move. If the move is in jump_moves, then we call make_jump_move which changes our 
-- GameState accordingly, and we also add our move to history and change the status to the next player after this function returns. If none of these guards 
-- pass, then the move must have been illegal, so we return the same GameState but with a new message telling the user the move was invalid. Also, if we 
-- call make_simple_move or make_jump_move, we also call isGameOver on our final GameState to see if the game is over or not.
apply_move :: Move -> GameState -> GameState
apply_move m g
    | moves g == ([], []) = g {status = GameOver}
    | m `elem` simple_moves g && jump_moves g == [] = (make_simple_move (convert m) g) {history = [m] ++ history g}
    | m `elem` simple_moves g && jump_moves g /= [] = g {message = "Illegal move! A jump is available:  " ++ (show (jump_moves g))}
    | m `elem` jump_moves g = (make_jump_move (convert m) g) {status = change_player g
                                                              , history = [m] ++ history g}
    | otherwise = g{message = "Illegal move!"}

-- This function applies a simple move to a GameState and returns a new GameState. We use guards for the cases when our piece is part of red/black pieces/kings.
-- In the cases of red/black kings, we replace our start Coord with our end Coord, switch the status to the next player, and change the message to "". In the
-- cases of red/black pieces, we first check if the piece became a king. If it did, we remove the piece from red/blackPieces, add it to red/blackKings, change
-- the player, and change the message to "". If the piece does not become a king, we replace the start Coord with the end Coord in red/blackPieces, change the
-- player, and change the message to "". If for some reason none of these guards are entered, we have a fail safe to report a GameState with the message 
-- invalid make_simple_move. Also, this functions idea and barebones came from Si Zhang's tutorial.
make_simple_move :: PieceState -> GameState -> GameState
make_simple_move [start,end] g
    | status g == (Turn Black) && elem start (blackKings g) 
        = g{blackKings = replace start end (blackKings g)
            , status = change_player g
            , message = ""}
    | status g == (Turn Black) && elem start (blackPieces g)
        = if is_king end (Turn Black)
            then g{blackPieces = remove start (blackPieces g)
                   , blackKings = end:(blackKings g)
                   , status = change_player g
                   , message = ""}
            else g{blackPieces = replace start end (blackPieces g)
                   , status = change_player g
                   , message = ""}
    | status g == (Turn Red) && elem start (redKings g)
        = g{redKings = replace start end (redKings g)
            , status = change_player g
            , message = ""}
    | status g == (Turn Red) && elem start (redPieces g)
        = if is_king end (Turn Red) 
            then g{redPieces = remove start (redPieces g)
                   , redKings = end:(redKings g)
                   , status = change_player g
                   , message = ""}
            else g{redPieces = replace start end (redPieces g)
                   , status = change_player g
                   , message = ""}
    | otherwise = g{message = "invalid make_simple_move"}

-- This function applies a jump move to a GameState and returns a new GameState. We use guards for the cases when our piece is part of red/black pieces/kings.
-- In the cases of red/black kings, we remove the piece we jumped from the opposite colour king/piece lists as we don't know what type of piece we are jumping, 
-- add the next Coord to our red/blackKings list and remove the start Coord from it, and change the message to "". In the cases of red/black pieces, we first
-- check if the piece became a king. If it did, we remove the piece from red/blackPieces, add it to red/blackKings, remove the piece we jumped from the opposite 
-- colour king/piece lists as we don't know what type of piece we are jumping, and finally change the message to "". If the piece does not become a king, we 
-- remove the start Coord and add the next Coord in red/blackPieces, remove the piece we jumped from the opposite colour king/piece lists as we don't know what 
-- type of piece we are jumping, and change the message to "". In all of these cases, we pass the "updated" GameState to make_jump_move with (next:rest), so 
-- that we keep using the function to update the jump move, as some moves can have multiple jumps. If for some reason none of these guards are entered, we have 
-- a fail safe to report a GameState with the message invalid make_jump_move. Finally, if the pattern match with (start:(next:rest)) fails, we have another 
-- pattern match to catch when the jump is finished. For example, if we have finished our jump and we have called make_jump_move with (next:rest), rest will
-- be [], and so the pattern macth with (start:(next:rest)) fails. So, we know when this match fails, the jump is finished and we return our GameState, which 
-- is what our final pattern match does. Also, this functions idea and barebones came from Si Zhang's tutorial.
make_jump_move :: PieceState -> GameState -> GameState
make_jump_move (start:(next:rest)) g 
    | status g == (Turn Black) && elem start (blackKings g) 
        = make_jump_move (next:rest)
                (g{redKings = remove (jumped start next) (redKings g)
                    , redPieces = remove (jumped start next) (redPieces g)
                    , blackKings = next:(remove start (blackKings g))
                    , message = ""})
    | status g == (Turn Black) && elem start (blackPieces g) 
        = if is_king next (Turn Black)
            then make_jump_move (next:rest)
                (g{blackPieces = remove start (blackPieces g)
                    , redKings = remove (jumped start next) (redKings g)
                    , redPieces = remove (jumped start next) (redPieces g)
                    , blackKings = next:(blackKings g)
                    , message = ""})
            else make_jump_move (next:rest)
                (g{blackPieces = next:(remove start (blackPieces g))
                    , redKings = remove (jumped start next) (redKings g)
                    , redPieces = remove (jumped start next) (redPieces g)
                    , message = ""})
    | status g == (Turn Red) && elem start (redKings g)
        = make_jump_move (next:rest)
                (g{blackKings = remove (jumped start next) (blackKings g)
                    , blackPieces = remove (jumped start next) (blackPieces g)
                    , redKings = next:(remove start (redKings g))
                    , message = ""})
    | status g == (Turn Red) && elem start (redPieces g)
        = if is_king next (Turn Red)
            then make_jump_move (next:rest)
                (g{redPieces = remove start (redPieces g)
                    , blackKings = remove (jumped start next) (blackKings g)
                    , blackPieces = remove (jumped start next) (blackPieces g)
                    , redKings = next:(redKings g)
                    , message = ""})
            else make_jump_move (next:rest)
                (g{redPieces = next:(remove start (redPieces g))
                    , blackKings = remove (jumped start next) (blackKings g)
                    , blackPieces = remove (jumped start next) (blackPieces g)
                    , message = ""})
    | otherwise = g{message = "invalid make_jump_move"}
make_jump_move _ g = g

-- This function determines if a Coord is a king. Since we take in a Status, if the Status is Turn Red and our y of our Coord is 0, then our piece is a red 
-- king, so we return true. If the Status is Turn Black and our y of our Coord is 7, then our piece is a black king, so we return true. Otherwise, our coord 
-- was not 0 or 7, and so we return false as it is not a king.
is_king :: Coord -> Status -> Bool
is_king (x, y) s
    | s == (Turn Red) && y == 0 = True
    | s == (Turn Black) && y == 7 = True
    | otherwise = False

-- This function simply takes in a two Coords, one we want to replace and the one that we are replacing it with, and the list of Coords we look to replace 
-- our Coord from. We use a simple list comprehension to check if the Coord we want to replace ever shows up, and if it does we replace it with the Coord
-- we want to. If its not, we just put the Coord back into the list.
replace :: Coord -> Coord -> [Coord] -> [Coord]
replace x y xs = [if (z == x) then y else z | z <- xs]

-- This function removes a Coord from a list of Coords. We use a list comprehension, with a condition that if the Coord from our list is not equal to the one
-- we want to remove, it can be in our list comprehension. So, when we encounter the Coord we want to remove, it fails the test and is not put into the list.
remove :: Coord -> [Coord] -> [Coord]
remove c xs = [x | x <- xs, x /= c]

-- This function changes the status of a GameState. If the Turn was Red, we change to Turn Black. Otherwise, the Turn was Black, and we change it to Turn Red.
change_player :: GameState -> Status
change_player g
    | status g == Turn Red = Turn Black
    | otherwise = Turn Red

-- This function finds the square on the board that is getting jumped, using the starting and ending Coords of the piece that jumped it. To find this, 
-- we simply average the x and y values of the start and end of the piece, and that gives us the Coords of the piece that was jumped.
jumped :: Coord -> Coord -> Coord
jumped (x, y) (x', y') = (xavg, yavg) where xavg = (x + x') `div` 2 
                                            yavg = (y + y') `div` 2

-- This function takes in a move and converts it to a PieceState, basically removing the PorK data from the move and leaving us only with the Coord data.
-- We use a list comprehension that goes through each part of the move and uses the change function to remove the PorK data as required.
convert :: Move -> PieceState
convert m =  [change x | x <- m]

-- This function takes in a PorK Coord, removes the PorK data and returns only the Coord data. We simply use pattern matching to cover cases of P and K, 
-- and only return the (x, y) values, or the Coord that we wanted.
change :: PorK Coord -> Coord
change (P (x, y)) = (x, y)
change (K (x, y)) = (x, y)