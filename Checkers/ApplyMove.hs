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

apply_move:: Move -> GameState -> GameState
apply_move mv st = st{message = "!!GAME OVER!!"}


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
    
