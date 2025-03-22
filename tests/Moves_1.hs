
module Moves where

--import Lens.Micro.Platform -- DON'T!!!

import GameLogic


-------------ADD YOUR PROGRAM FOR MOVES-------------
--------------------BELOW HERE----------------------


--DECIDE AND DETERMINE IF MOVE IS LEGAL
--all possible moves
moves::GameState -> [Move]
moves s 
 | (is_there (jump_moves s)) 
  = (jump_moves s)
 | otherwise = simple_moves s
--all possible simple moves
simple_moves::GameState -> [Move]
simple_moves s = case (_status s) of
 Red -> rsimple_moves s
 Black -> bsimple_moves s
 _ -> []

--all possible simple moves for red
rsimple_moves::GameState-> [Move]
rsimple_moves s = (rpawn_moves s  (_redPieces s) [])++(rking_moves s (_redKings s) [])

--all possible simple moves for red pawns
rpawn_moves::GameState->[Coord]->[Move]->[Move]
rpawn_moves s [] m = m
rpawn_moves s ((x,y):rest) m
 |(is_up_right_empty s (x,y)) && (is_up_left_empty s (x,y))
  = rpawn_moves s rest ([(x,y),(x-1,y-1)]:[(x,y),(x+1,y-1)]:m)
 |(is_up_left_empty s (x,y))
  = rpawn_moves s rest ([(x,y),(x-1,y-1)]: m)
 |(is_up_right_empty s (x,y))
  = rpawn_moves s rest ([(x,y),(x+1,y-1)]: m)
 |otherwise = rpawn_moves s rest m
 
--all possible simple moves for red kings
rking_moves::GameState->[Coord]->[Move]->[Move]
rking_moves s [] m = m
rking_moves s ((x,y):rest) m 
 |(is_up_right_empty s (x,y)) && (is_up_left_empty s (x,y)) && (is_down_right_empty s (x,y)) && (is_down_left_empty s (x,y))
  = rking_moves s rest ([(x,y),(x+1,y-1)]:[(x,y),(x-1,y-1)]:[(x,y),(x+1,y+1)]:[(x,y),(x-1,y+1)]:m)
 |(is_up_right_empty s (x,y)) && (is_up_left_empty s (x,y)) && (is_down_right_empty s (x,y))
  = rking_moves s rest ([(x,y),(x+1,y-1)]:[(x,y),(x-1,y-1)]:[(x,y),(x+1,y+1)]:m)
 |(is_up_right_empty s (x,y)) && (is_up_left_empty s (x,y))  && (is_down_left_empty s (x,y))
  = rking_moves s rest ([(x,y),(x+1,y-1)]:[(x,y),(x-1,y-1)]:[(x,y),(x-1,y+1)]:m)
 |(is_up_right_empty s (x,y)) && (is_up_left_empty s (x,y)) 
  = rking_moves s rest ([(x,y),(x+1,y-1)]:[(x,y),(x-1,y-1)]:m)
 |(is_up_right_empty s (x,y)) && (is_down_right_empty s (x,y)) && (is_down_left_empty s (x,y))
  = rking_moves s rest ([(x,y),(x+1,y-1)]:[(x,y),(x+1,y+1)]:[(x,y),(x-1,y+1)]:m)
 |(is_up_right_empty s (x,y))  && (is_down_right_empty s (x,y))
  = rking_moves s rest ([(x,y),(x+1,y-1)]:[(x,y),(x+1,y+1)]:m)
 |(is_up_right_empty s (x,y)) &&  (is_down_left_empty s (x,y))
  = rking_moves s rest ([(x,y),(x+1,y-1)]:[(x,y),(x-1,y+1)]:m)
 |(is_up_right_empty s (x,y)) 
  = rking_moves s rest ([(x,y),(x+1,y-1)]:m)
 |(is_up_left_empty s (x,y)) && (is_down_right_empty s (x,y)) && (is_down_left_empty s (x,y))
  = rking_moves s rest ([(x,y),(x-1,y-1)]:[(x,y),(x+1,y+1)]:[(x,y),(x-1,y+1)]:m)
 |(is_up_left_empty s (x,y)) && (is_down_right_empty s (x,y)) 
  = rking_moves s rest ([(x,y),(x-1,y-1)]:[(x,y),(x+1,y+1)]:m)
 |(is_up_left_empty s (x,y)) &&  (is_down_left_empty s (x,y))
  = rking_moves s rest ([(x,y),(x-1,y-1)]:[(x,y),(x-1,y+1)]:m)
 |(is_up_left_empty s (x,y)) 
  = rking_moves s rest ([(x,y),(x-1,y-1)]:m)
 |(is_down_right_empty s (x,y)) && (is_down_left_empty s (x,y))
  = rking_moves s rest ([(x,y),(x+1,y+1)]:[(x,y),(x-1,y+1)]:m)
 |(is_down_right_empty s (x,y))
  = rking_moves s rest ([(x,y),(x+1,y+1)]:m)
  |(is_down_left_empty s (x,y))
  = rking_moves s rest ([(x,y),(x-1,y+1)]:m)
 |otherwise = rking_moves s rest m
 
 
--all possible simple moves for black
bsimple_moves::GameState->[Move]
bsimple_moves s = (bpawn_moves s  (_blackPieces s) [])++(bking_moves s (_blackKings s) [])

bpawn_moves::GameState->[Coord]->[Move]->[Move]
bpawn_moves s [] m = m
bpawn_moves s ((x,y):rest) m
 |(is_down_right_empty s (x,y)) && (is_down_left_empty s (x,y))
  = bpawn_moves s rest ([(x,y),(x-1,y+1)]:[(x,y),(x+1,y+1)]:m)
 |(is_down_left_empty s (x,y))
  = bpawn_moves s rest ([(x,y),(x-1,y+1)]: m)
 |(is_down_right_empty s (x,y))
  = bpawn_moves s rest ([(x,y),(x+1,y+1)]: m)
 |otherwise = bpawn_moves s rest m
 
--all possible blacck king simple moves
bking_moves::GameState->[Coord]->[Move]->[Move]
bking_moves s [] m = m
bking_moves s ((x,y):rest) m 
 |(is_up_right_empty s (x,y)) && (is_up_left_empty s (x,y)) && (is_down_right_empty s (x,y)) && (is_down_left_empty s (x,y))
  = bking_moves s rest ([(x,y),(x+1,y-1)]:[(x,y),(x-1,y-1)]:[(x,y),(x+1,y+1)]:[(x,y),(x-1,y+1)]:m)
 |(is_up_right_empty s (x,y)) && (is_up_left_empty s (x,y)) && (is_down_right_empty s (x,y))
  = bking_moves s rest ([(x,y),(x+1,y-1)]:[(x,y),(x-1,y-1)]:[(x,y),(x+1,y+1)]:m)
 |(is_up_right_empty s (x,y)) && (is_up_left_empty s (x,y))  && (is_down_left_empty s (x,y))
  = bking_moves s rest ([(x,y),(x+1,y-1)]:[(x,y),(x-1,y-1)]:[(x,y),(x-1,y+1)]:m)
 |(is_up_right_empty s (x,y)) && (is_up_left_empty s (x,y)) 
  = bking_moves s rest ([(x,y),(x+1,y-1)]:[(x,y),(x-1,y-1)]:m)
 |(is_up_right_empty s (x,y)) && (is_down_right_empty s (x,y)) && (is_down_left_empty s (x,y))
  = bking_moves s rest ([(x,y),(x+1,y-1)]:[(x,y),(x+1,y+1)]:[(x,y),(x-1,y+1)]:m)
 |(is_up_right_empty s (x,y))  && (is_down_right_empty s (x,y))
  = bking_moves s rest ([(x,y),(x+1,y-1)]:[(x,y),(x+1,y+1)]:m)
 |(is_up_right_empty s (x,y)) &&  (is_down_left_empty s (x,y))
  = bking_moves s rest ([(x,y),(x+1,y-1)]:[(x,y),(x-1,y+1)]:m)
 |(is_up_right_empty s (x,y)) 
  = bking_moves s rest ([(x,y),(x+1,y-1)]:m)
 |(is_up_left_empty s (x,y)) && (is_down_right_empty s (x,y)) && (is_down_left_empty s (x,y))
  = bking_moves s rest ([(x,y),(x-1,y-1)]:[(x,y),(x+1,y+1)]:[(x,y),(x-1,y+1)]:m)
 |(is_up_left_empty s (x,y)) && (is_down_right_empty s (x,y)) 
  = bking_moves s rest ([(x,y),(x-1,y-1)]:[(x,y),(x+1,y+1)]:m)
 |(is_up_left_empty s (x,y)) &&  (is_down_left_empty s (x,y))
  = bking_moves s rest ([(x,y),(x-1,y-1)]:[(x,y),(x-1,y+1)]:m)
 |(is_up_left_empty s (x,y)) 
  = bking_moves s rest ([(x,y),(x-1,y-1)]:m)
 |(is_down_right_empty s (x,y)) && (is_down_left_empty s (x,y))
  = bking_moves s rest ([(x,y),(x+1,y+1)]:[(x,y),(x-1,y+1)]:m)
 |(is_down_right_empty s (x,y))
  = bking_moves s rest ([(x,y),(x+1,y+1)]:m)
  |(is_down_left_empty s (x,y))
  = bking_moves s rest ([(x,y),(x-1,y+1)]:m)
 |otherwise = bking_moves s rest m


--DETERMINE jump_moves in a certain gamestate

jump_moves::GameState -> [Move]
jump_moves s = case (_status s) of
 Red ->  (r_alljumps s (_redPieces s) []) ++ (rk_alljumps s (_redKings s) [])
 Black -> (b_alljumps s (_blackPieces s) []) ++ (bk_alljumps s (_blackKings s) [])
 _ -> []

r_alljumps::GameState->[Coord]->[Move]->[Move]
r_alljumps s [] rem = rem
r_alljumps s ((x,y):xs) rem 
 | (r_jumps s (x,y) []) == [[(x,y)]]
  = (r_alljumps s xs rem)
 | otherwise
  = (r_alljumps s xs rem ++ (r_jumps s (x,y) []))


r_jumps::GameState->Coord->Move->[Move]
r_jumps s (x,y) rem
 | (is_up_left_jump_possible s (x,y)) && (is_up_right_jump_possible s (x,y)) && (y-2 == 0)
  = (rking_jumps (removeComponent s (x,y) (x-2,y-2)) (x-2,y-2) (rem++[(x,y)]))++ (rking_jumps (removeComponent s (x,y) (x+2,y-2)) (x+2,y-2) (rem++[(x,y)]))
 | (is_up_left_jump_possible s (x,y)) && (is_up_right_jump_possible s (x,y)) 
  = (r_jumps s (x-2,y-2) (rem++[(x,y)]))++ (r_jumps s (x+2,y-2) (rem++[(x,y)]))
 | (is_up_right_jump_possible s (x,y)) && (y-2 == 0) 
 = rking_jumps (removeComponent s (x,y) (x+2,y-2)) (x+2,y-2) (rem++[(x,y)])
 | (is_up_right_jump_possible s (x,y)) 
 = r_jumps s (x+2,y-2) (rem++[(x,y)]) 
 | (is_up_left_jump_possible s (x,y)) && (y-2 == 0)
 = rking_jumps (removeComponent s (x,y) (x-2,y-2)) (x-2,y-2) (rem++[(x,y)]) 
 | (is_up_left_jump_possible s (x,y))
 = r_jumps s (x-2,y-2) (rem++[(x,y)])
 |otherwise = [rem ++ [(x,y)]]

rk_alljumps::GameState->[Coord]->[Move]->[Move]
rk_alljumps s [] rem = rem
rk_alljumps s ((x,y):xs) rem 
 | (rking_jumps s (x,y) []) == [[(x,y)]]
  = (rk_alljumps s xs rem)
 | otherwise
  = (rk_alljumps s xs rem ++ (rking_jumps s (x,y) []))

rking_jumps::GameState->Coord->Move->[Move]
rking_jumps s (x,y) rem
 | (is_up_left_jump_possible s (x,y)) && (is_up_right_jump_possible s (x,y)) && (is_down_right_jump_possible s (x,y)) && (is_down_left_jump_possible s (x,y))
  = (rking_jumps (removeComponent s (x,y) (x-2,y-2)) (x-2,y-2) (rem++[(x,y)])) ++ (rking_jumps (removeComponent s (x,y) (x+2,y-2)) (x+2,y-2) (rem++[(x,y)])) ++ (rking_jumps (removeComponent s (x,y) (x+2,y+2)) (x+2,y+2) (rem++[(x,y)])) ++ (rking_jumps (removeComponent s (x,y) (x-2,y+2)) (x-2,y+2) (rem++[(x,y)]))
 | (is_up_left_jump_possible s (x,y)) && (is_up_right_jump_possible s (x,y)) && (is_down_right_jump_possible s (x,y))
  = (rking_jumps (removeComponent s (x,y) (x-2,y-2)) (x-2,y-2) (rem++[(x,y)])) ++ (rking_jumps (removeComponent s (x,y) (x+2,y-2)) (x+2,y-2) (rem++[(x,y)])) ++ (rking_jumps (removeComponent s (x,y) (x+2,y+2)) (x+2,y+2) (rem++[(x,y)]))
 | (is_up_left_jump_possible s (x,y)) && (is_up_right_jump_possible s (x,y)) && (is_down_left_jump_possible s (x,y))
  = (rking_jumps (removeComponent s (x,y) (x-2,y-2)) (x-2,y-2) (rem++[(x,y)])) ++ (rking_jumps (removeComponent s (x,y) (x+2,y-2)) (x+2,y-2) (rem++[(x,y)])) ++ (rking_jumps (removeComponent s (x,y) (x-2,y+2)) (x-2,y+2) (rem++[(x,y)]))
 | (is_up_left_jump_possible s (x,y)) && (is_up_right_jump_possible s (x,y)) 
  = (rking_jumps (removeComponent s (x,y) (x-2,y-2)) (x-2,y-2) (rem++[(x,y)])) ++ (rking_jumps (removeComponent s (x,y) (x+2,y-2)) (x+2,y-2) (rem++[(x,y)])) 
 | (is_up_left_jump_possible s (x,y)) && (is_down_right_jump_possible s (x,y)) && (is_down_left_jump_possible s (x,y))
  = (rking_jumps (removeComponent s (x,y) (x-2,y-2)) (x-2,y-2) (rem++[(x,y)])) ++ (rking_jumps (removeComponent s (x,y) (x+2,y+2)) (x+2,y+2) (rem++[(x,y)])) ++ (rking_jumps (removeComponent s (x,y) (x-2,y+2)) (x-2,y+2) (rem++[(x,y)]))
 | (is_up_left_jump_possible s (x,y)) && (is_down_right_jump_possible s (x,y)) 
  = (rking_jumps (removeComponent s (x,y) (x-2,y-2)) (x-2,y-2) (rem++[(x,y)])) ++ (rking_jumps (removeComponent s (x,y) (x+2,y+2)) (x+2,y+2) (rem++[(x,y)]))
 | (is_up_left_jump_possible s (x,y)) && (is_down_left_jump_possible s (x,y)) 
  = (rking_jumps (removeComponent s (x,y) (x-2,y-2)) (x-2,y-2) (rem++[(x,y)])) ++ (rking_jumps (removeComponent s (x,y) (x-2,y+2)) (x-2,y+2) (rem++[(x,y)]))
 | (is_up_left_jump_possible s (x,y)) 
  = (rking_jumps (removeComponent s (x,y) (x-2,y-2)) (x-2,y-2) (rem++[(x,y)])) 
 | (is_up_right_jump_possible s (x,y)) && (is_down_right_jump_possible s (x,y)) && (is_down_left_jump_possible s (x,y))
  = (rking_jumps (removeComponent s (x,y) (x+2,y-2)) (x+2,y-2) (rem++[(x,y)])) ++ (rking_jumps (removeComponent s (x,y) (x+2,y+2)) (x+2,y+2) (rem++[(x,y)])) ++ (rking_jumps (removeComponent s (x,y) (x-2,y+2)) (x-2,y+2) (rem++[(x,y)]))
 | (is_up_right_jump_possible s (x,y)) && (is_down_right_jump_possible s (x,y)) 
  = (rking_jumps (removeComponent s (x,y) (x+2,y-2)) (x+2,y-2) (rem++[(x,y)])) ++ (rking_jumps (removeComponent s (x,y) (x+2,y+2)) (x+2,y+2) (rem++[(x,y)]))
 | (is_up_right_jump_possible s (x,y)) && (is_down_left_jump_possible s (x,y)) 
  = (rking_jumps (removeComponent s (x,y) (x+2,y-2)) (x+2,y-2) (rem++[(x,y)])) ++ (rking_jumps (removeComponent s (x,y) (x-2,y+2)) (x-2,y+2) (rem++[(x,y)]))
 | (is_up_right_jump_possible s (x,y)) 
  = (rking_jumps (removeComponent s (x,y) (x+2,y-2)) (x+2,y-2) (rem++[(x,y)]))
 | (is_down_right_jump_possible s (x,y)) && (is_down_left_jump_possible s (x,y)) 
  = (rking_jumps (removeComponent s (x,y) (x+2,y+2)) (x+2,y+2) (rem++[(x,y)])) ++ (rking_jumps (removeComponent s (x,y) (x-2,y+2)) (x-2,y+2) (rem++[(x,y)]))
 | (is_down_right_jump_possible s (x,y))  
  = (rking_jumps (removeComponent s (x,y) (x+2,y+2)) (x+2,y+2) (rem++[(x,y)])) 
 | (is_down_left_jump_possible s (x,y))  
  = (rking_jumps (removeComponent s (x,y) (x-2,y+2)) (x-2,y+2) (rem++[(x,y)])) 
 |otherwise = [rem ++ [(x,y)]]



b_alljumps::GameState->[Coord]->[Move]->[Move]
b_alljumps s [] rem = rem
b_alljumps s ((x,y):xs) rem
 | (b_jumps s (x,y) []) == [[(x,y)]]
  = (b_alljumps s xs rem)
 | otherwise
  = (b_alljumps s xs rem ++ (b_jumps s (x,y) []))


b_jumps::GameState->Coord->Move->[Move]
b_jumps s (x,y) rem
 | (is_down_left_jump_possible s (x,y)) && (is_down_right_jump_possible s (x,y)) && (y+2 == 7)
  = (bking_jumps (removeComponent s (x,y) (x-2,y+2)) (x-2,y+2) (rem++[(x,y)]))++ (bking_jumps (removeComponent s (x,y) (x+2,y-2)) (x+2,y-2) (rem++[(x,y)]))
 | (is_down_left_jump_possible s (x,y)) && (is_down_right_jump_possible s (x,y))
  = (b_jumps s (x-2,y+2) (rem++[(x,y)]))++ (b_jumps s (x+2,y-2) (rem++[(x,y)]))
 | (is_down_right_jump_possible s (x,y)) && (y+2 ==7) 
 = bking_jumps (removeComponent s (x,y) (x+2,y+2)) (x+2,y+2) (rem++[(x,y)]) 
 | (is_down_right_jump_possible s (x,y)) 
 = b_jumps s (x+2,y+2) (rem++[(x,y)]) 
 | (is_down_left_jump_possible s (x,y)) && (y+2 == 7)
 = bking_jumps (removeComponent s (x,y) (x-2,y+2)) (x-2,y+2) (rem++[(x,y)]) 
 | (is_down_left_jump_possible s (x,y))
 = b_jumps s (x-2,y+2) (rem++[(x,y)]) 
 |otherwise = [rem ++ [(x,y)]]

bk_alljumps::GameState->[Coord]->[Move]->[Move]
bk_alljumps s [] rem = rem
bk_alljumps s ((x,y):xs) rem 
 | (bking_jumps s (x,y) []) == [[(x,y)]]
  = (bk_alljumps s xs rem)
 | otherwise
  = (bk_alljumps s xs rem ++ (bking_jumps s (x,y) []))

bking_jumps::GameState->Coord->Move->[Move]
bking_jumps s (x,y) rem
 | (is_up_left_jump_possible s (x,y)) && (is_up_right_jump_possible s (x,y)) && (is_down_right_jump_possible s (x,y)) && (is_down_left_jump_possible s (x,y))
  = (bking_jumps (removeComponent s (x,y) (x-2,y-2)) (x-2,y-2) (rem++[(x,y)])) ++ (bking_jumps (removeComponent s (x,y) (x+2,y-2)) (x+2,y-2) (rem++[(x,y)])) ++ (bking_jumps (removeComponent s (x,y) (x+2,y+2)) (x+2,y+2) (rem++[(x,y)])) ++ (bking_jumps (removeComponent s (x,y) (x-2,y+2)) (x-2,y+2) (rem++[(x,y)]))
 | (is_up_left_jump_possible s (x,y)) && (is_up_right_jump_possible s (x,y)) && (is_down_right_jump_possible s (x,y))
  = (bking_jumps (removeComponent s (x,y) (x-2,y-2)) (x-2,y-2) (rem++[(x,y)])) ++ (bking_jumps (removeComponent s (x,y) (x+2,y-2)) (x+2,y-2) (rem++[(x,y)])) ++ (bking_jumps (removeComponent s (x,y) (x+2,y+2)) (x+2,y+2) (rem++[(x,y)]))
 | (is_up_left_jump_possible s (x,y)) && (is_up_right_jump_possible s (x,y)) && (is_down_left_jump_possible s (x,y))
  = (bking_jumps (removeComponent s (x,y) (x-2,y-2)) (x-2,y-2) (rem++[(x,y)])) ++ (bking_jumps (removeComponent s (x,y) (x+2,y-2)) (x+2,y-2) (rem++[(x,y)])) ++ (bking_jumps (removeComponent s (x,y) (x-2,y+2)) (x-2,y+2) (rem++[(x,y)]))
 | (is_up_left_jump_possible s (x,y)) && (is_up_right_jump_possible s (x,y)) 
  = (bking_jumps (removeComponent s (x,y) (x-2,y-2)) (x-2,y-2) (rem++[(x,y)])) ++ (bking_jumps (removeComponent s (x,y) (x+2,y-2)) (x+2,y-2) (rem++[(x,y)])) 
 | (is_up_left_jump_possible s (x,y)) && (is_down_right_jump_possible s (x,y)) && (is_down_left_jump_possible s (x,y))
  = (bking_jumps (removeComponent s (x,y) (x-2,y-2)) (x-2,y-2) (rem++[(x,y)])) ++ (bking_jumps (removeComponent s (x,y) (x+2,y+2)) (x+2,y+2) (rem++[(x,y)])) ++ (bking_jumps (removeComponent s (x,y) (x-2,y+2)) (x-2,y+2) (rem++[(x,y)]))
 | (is_up_left_jump_possible s (x,y)) && (is_down_right_jump_possible s (x,y)) 
  = (bking_jumps (removeComponent s (x,y) (x-2,y-2)) (x-2,y-2) (rem++[(x,y)])) ++ (bking_jumps (removeComponent s (x,y) (x+2,y+2)) (x+2,y+2) (rem++[(x,y)]))
 | (is_up_left_jump_possible s (x,y)) && (is_down_left_jump_possible s (x,y)) 
  = (bking_jumps (removeComponent s (x,y) (x-2,y-2)) (x-2,y-2) (rem++[(x,y)])) ++ (bking_jumps (removeComponent s (x,y) (x-2,y+2)) (x-2,y+2) (rem++[(x,y)]))
 | (is_up_left_jump_possible s (x,y)) 
  = (bking_jumps (removeComponent s (x,y) (x-2,y-2)) (x-2,y-2) (rem++[(x,y)])) 
 | (is_up_right_jump_possible s (x,y)) && (is_down_right_jump_possible s (x,y)) && (is_down_left_jump_possible s (x,y))
  = (bking_jumps (removeComponent s (x,y) (x+2,y-2)) (x+2,y-2) (rem++[(x,y)])) ++ (bking_jumps (removeComponent s (x,y) (x+2,y+2)) (x+2,y+2) (rem++[(x,y)])) ++ (bking_jumps (removeComponent s (x,y) (x-2,y+2)) (x-2,y+2) (rem++[(x,y)]))
 | (is_up_right_jump_possible s (x,y)) && (is_down_right_jump_possible s (x,y)) 
  = (bking_jumps (removeComponent s (x,y) (x+2,y-2)) (x+2,y-2) (rem++[(x,y)])) ++ (bking_jumps (removeComponent s (x,y) (x+2,y+2)) (x+2,y+2) (rem++[(x,y)]))
 | (is_up_right_jump_possible s (x,y)) && (is_down_left_jump_possible s (x,y)) 
  = (bking_jumps (removeComponent s (x,y) (x+2,y-2)) (x+2,y-2) (rem++[(x,y)])) ++ (bking_jumps (removeComponent s (x,y) (x-2,y+2)) (x-2,y+2) (rem++[(x,y)]))
 | (is_up_right_jump_possible s (x,y)) 
  = (bking_jumps (removeComponent s (x,y) (x+2,y-2)) (x+2,y-2) (rem++[(x,y)]))
 | (is_down_right_jump_possible s (x,y)) && (is_down_left_jump_possible s (x,y)) 
  = (bking_jumps (removeComponent s (x,y) (x+2,y+2)) (x+2,y+2) (rem++[(x,y)])) ++ (bking_jumps (removeComponent s (x,y) (x-2,y+2)) (x-2,y+2) (rem++[(x,y)]))
 | (is_down_right_jump_possible s (x,y))  
  = (bking_jumps (removeComponent s (x,y) (x+2,y+2)) (x+2,y+2) (rem++[(x,y)])) 
 | (is_down_left_jump_possible s (x,y))  
  = (bking_jumps (removeComponent s (x,y) (x-2,y+2)) (x-2,y+2) (rem++[(x,y)])) 
 |otherwise = [rem ++ [(x,y)]]




--HELPER FUNCTIONS--

--Diagonal simple movement helpers---------------
is_up_right_empty::GameState -> Coord -> Bool
is_up_right_empty s (x,y)
 |not((x+1, y-1) `elem` (_redPieces s)) &&
  not((x+1, y-1) `elem` (_redKings s)) &&
  not((x+1, y-1) `elem` (_blackPieces s)) &&
  not((x+1, y-1) `elem` (_blackKings s)) &&
  (x+1 <= 7) && (y-1 >= 0)
  = True
 |otherwise = False

is_up_left_empty::GameState -> Coord -> Bool
is_up_left_empty s (x,y)
 |not((x-1, y-1) `elem` (_redPieces s)) &&
  not((x-1, y-1) `elem` (_redKings s)) &&
  not((x-1, y-1) `elem` (_blackPieces s)) &&
  not((x-1, y-1) `elem` (_blackKings s)) &&
  (x-1 >= 0) && (y-1 >=0)
  = True
 |otherwise = False

is_down_right_empty::GameState -> Coord -> Bool
is_down_right_empty s (x,y)
 |not((x+1, y+1) `elem` (_redPieces s)) &&
  not((x+1, y+1) `elem` (_redKings s)) &&
  not((x+1, y+1) `elem` (_blackPieces s)) &&
  not((x+1, y+1) `elem` (_blackKings s)) &&
  (x+1 <= 7) && (y+1 <= 7)
  = True
 |otherwise = False

is_down_left_empty::GameState -> Coord -> Bool
is_down_left_empty s (x,y)
 |not((x-1, y+1) `elem` (_redPieces s)) &&
  not((x-1, y+1) `elem` (_redKings s)) &&
  not((x-1, y+1) `elem` (_blackPieces s)) &&
  not((x-1, y+1) `elem` (_blackKings s)) &&
  (x-1 >= 0) && (y+1 <= 7)
  = True
 |otherwise = False
------------------------------------------


--DIAGONAL JUMP HELPERS-------------------

opponent_occupied::Coord->GameState->Bool
opponent_occupied (x,y) s
 | (_status s) == Red
 = if(((x, y) `elem` (_blackPieces s)) || ((x, y) `elem` (_blackKings s)))
    then True
    else False
 |(_status s) == Black
 = if(((x, y) `elem` (_redPieces s)) || ((x, y) `elem` (_redKings s)))
    then True
    else False
 |otherwise = False

onboard::Coord->Bool
onboard (x,y)
 |(x>=0) && (x<=7) && (y<=7) && (x>=0)
  = True
 | otherwise = False

is_up_right_jump_possible::GameState-> Coord ->Bool
is_up_right_jump_possible s (x,y)
 |(_status s == Red) &&
  not((x+2, y-2) `elem` (_redPieces s)) &&
  not((x+2, y-2) `elem` (_redKings s)) &&
  not((x+2, y-2) `elem` (_blackPieces s)) &&
  not((x+2, y-2) `elem` (_blackKings s)) &&
  (x+2 <= 7) && (y-2 >= 0) &&
  ((x+1, y-1) `elem` (_blackPieces s) || (x+1, y-1) `elem` (_blackKings s))
  = True
  |(_status s == Black) &&
  not((x+2, y-2) `elem` (_redPieces s)) &&
  not((x+2, y-2) `elem` (_redKings s)) &&
  not((x+2, y-2) `elem` (_blackPieces s)) &&
  not((x+2, y-2) `elem` (_blackKings s)) &&
  (x+2 <= 7) && (y-2 >= 0) &&
  ((x+1, y-1) `elem` (_redPieces s) || (x+1, y-1) `elem` (_redKings s ))
  = True
  |otherwise = False

is_up_left_jump_possible::GameState-> Coord ->Bool
is_up_left_jump_possible s (x,y)
 |(_status s == Red) && 
  not((x-2, y-2) `elem` (_redPieces s)) &&
  not((x-2, y-2) `elem` (_redKings s)) &&
  not((x-2, y-2) `elem` (_blackPieces s)) &&
  not((x-2, y-2) `elem` (_blackKings s)) &&
  (x-2 >= 0) && (y-2 >= 0) &&
  ((x-1, y-1) `elem` (_blackPieces s) || (x-1, y-1) `elem` (_blackKings s))
  = True
 |(_status s == Black) && 
  not((x-2, y-2) `elem` (_redPieces s)) &&
  not((x-2, y-2) `elem` (_redKings s)) &&
  not((x-2, y-2) `elem` (_blackPieces s)) &&
  not((x-2, y-2) `elem` (_blackKings s)) &&
  (x-2 >= 0) && (y-2 >= 0) &&
  ((x-1, y-1) `elem` (_redPieces s ) || (x-1, y-1) `elem` (_redKings s))
  = True
  |otherwise = False
  
is_down_left_jump_possible::GameState-> Coord ->Bool
is_down_left_jump_possible s (x,y)
 |(_status s == Red) && 
  not((x-2, y+2) `elem` (_redPieces s)) &&
  not((x-2, y+2) `elem` (_redKings s)) &&
  not((x-2, y+2) `elem` (_blackPieces s)) &&
  not((x-2, y+2) `elem` (_blackKings s)) &&
  (x-2 >= 0) && (y+2 <= 7) &&
  ((x-1, y+1) `elem` (_blackPieces s) || (x-1, y+1) `elem` (_blackKings s))
  = True
 |(_status s == Black) && 
  not((x-2, y+2) `elem` (_redPieces s)) &&
  not((x-2, y+2) `elem` (_redKings s)) &&
  not((x-2, y+2) `elem` (_blackPieces s)) &&
  not((x-2, y+2) `elem` (_blackKings s)) &&
  (x-2 >= 0) && (y+2 <= 7) &&
  ((x-1, y+1) `elem` (_redPieces s ) || (x-1, y+1) `elem` (_redKings s ))
  = True
  |otherwise = False

is_down_right_jump_possible::GameState-> Coord ->Bool
is_down_right_jump_possible s (x,y)
 |(_status s == Red) &&
  not((x+2, y+2) `elem` (_redPieces s)) &&
  not((x+2, y+2) `elem` (_redKings s)) &&
  not((x+2, y+2) `elem` (_blackPieces s)) &&
  not((x+2, y+2) `elem` (_blackKings s)) &&
  (x+2 <= 7) && (y+2 <= 7) &&
  ((x+1, y+1) `elem` (_blackPieces s) || (x+1, y+1) `elem` (_blackKings s))
  = True
  |(_status s == Black) &&
  not((x+2, y+2) `elem` (_redPieces s)) &&
  not((x+2, y+2) `elem` (_redKings s))  &&
  not((x+2, y+2) `elem` (_blackPieces s)) &&
  not((x+2, y+2) `elem` (_blackKings s)) &&
  (x+2 <= 7) && (y+2 <= 7) &&
  ((x+1, y+1) `elem` (_redPieces s ) || (x+1, y+1) `elem` (_redKings s))
  = True
  |otherwise = False
------------------------------------------
removePiece:: [Coord] -> Coord -> [Coord]
removePiece [] _  = []
removePiece l c =  filter (\r ->  compareCoord r c ) l


removeComponent::GameState->Coord->Coord->GameState
removeComponent s (x,y) (x2,y2)
 |_status s == Red
 = s{_blackPieces = removePiece (_blackPieces s) (mid_jump_coord (x,y) (x2,y2))
   , _redPieces = removePiece (_redPieces s) (x,y)
   , _redKings  = removePiece (_redKings s)  (x,y)
   , _blackKings =  removePiece (_blackKings s) (mid_jump_coord (x,y) (x2,y2))
   }
 |_status s == Black
  = s{_redPieces =removePiece (_redPieces s) (mid_jump_coord (x,y) (x2,y2))
   , _blackPieces = removePiece (_blackPieces s) (x,y)
   , _blackKings  = removePiece (_blackKings s)  (x,y)
   , _redKings =  removePiece (_redKings s) (mid_jump_coord (x,y) (x2,y2))
   }
  | otherwise = s

mid_jump_coord::Coord->Coord->Coord
mid_jump_coord (x,y) (x2,y2) = (((x+x2)`div` 2),((y+y2)`div`2))


firstRow = [(0,0), (1,0),(2,0), (3,0),(4,0) ,(5,0),(6,0), (7,0)]
lastRow  = [ (0,7), (1,7),(2,7),(3,7), (4,7), (5,7),(6,7), (7,7)]
compareCoord:: Coord-> Coord -> Bool
compareCoord (x,y) (m,p) 
 | x==m && y==p = False
 | otherwise = True

is_there::[a]->Bool
is_there [] = False
is_there ms = True


isOdd :: Coord -> Bool
isOdd (x,y) = (mod (x+y) 2) == 0
