module Checkers.Moves where

import Checkers.Types

-- Implement your code for moves function below
moves:: GameState -> (SMorJM [Move])
moves game = EndM

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


