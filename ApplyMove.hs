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