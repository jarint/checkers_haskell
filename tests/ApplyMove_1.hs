module ApplyMove where

import Lens.Micro.Platform
import GameLogic
import Moves
applyTHEMove :: Move -> GameState -> GameState
applyTHEMove m s = case (_status s) of
 Red -> if (not((islegal s m))&&(is_there (jump_moves s)))
         then s{ _message = "There is a jump move available"}
         else if ((islegal s m)&&(m `elem` (simple_moves s))) 
         then if ((head m)`elem`(_redPieces s)) 
               then toogle_status $ apply_simple_move s m
               else toogle_status $ applyK_simple_move s m
         else if ((islegal s m)&&(m `elem` (jump_moves s)))
            then if ((head m)`elem`(_redPieces s) || (head m)`elem`(_redKings s))
                  then toogle_status $ apply_jump_move m s
                  else toogle_status $ apply_jump_move m s
        else s{ _message= "Invalid move" }
 Black -> if (not((islegal s m)) &&(is_there (jump_moves s)))
           then s{ _message = "There is a jump move available"}
           else if ((islegal s m)&&(m `elem` (simple_moves s))) 
           then if ((head m)`elem`(_blackPieces s)) 
               then toogle_status $ apply_simple_move s m
               else toogle_status $ applyK_simple_move s m
          else if ((islegal s m)&&(m `elem` (jump_moves s)))
           then if ((head m)`elem`(_blackPieces s) || (head m)`elem`(_blackKings s))
               then toogle_status $ apply_jump_move m s
               else toogle_status $ apply_jump_move m s
        else s{ _message= "Invalid move" }
 _ -> initialGameState


islegal::GameState->Move->Bool
islegal s [] = False
islegal s m
 | m `elem` (moves s)  = True 
 | otherwise = False


--Movement helper functions-------------------------------
apply_simple_move::GameState->Move->GameState
apply_simple_move s m = case (_status s) of
 Red-> if((head(tail m)) `elem` fRow)
        then s{_message = "Black turn",
               _status = Black ,
               _redPieces = ((removePiece (_redPieces s) (head m))) ,
               _redKings = ((head(tail m)):(_redKings s)) }
        else s{_message = "Black turn" , 
               _status = Black ,
               _redPieces = ((head(tail m)):(removePiece (_redPieces s) (head m))) }
 Black->if((head(tail m)) `elem` lRow)
        then s{ _message = "Red turn" ,
                _status = Red ,
                _blackPieces = ((removePiece (_blackPieces s) (head m))) ,
                _blackKings = ((head(tail m)):(_blackKings s)) }
        else s{_message = "Red turn " ,
               _status = Red ,
               _blackPieces =((head(tail m)):(removePiece (_blackPieces s) (head m))) }

applyK_simple_move::GameState->Move->GameState
applyK_simple_move s m = case (_status s) of
 Red-> s{_message = "Black turn" , 
         _status = Black ,
         _redKings = ((head(tail m)):(removePiece (_redKings s) (head m))) }
 Black-> s{ _message = "Red turn",
            _status = Red ,
            _blackKings = ((head(tail m)):(removePiece (_blackKings s) (head m))) }

apply_jump_move::Move->GameState->GameState
apply_jump_move [] s = s
apply_jump_move ((x,y):[]) s = s
apply_jump_move ((x,y):(x2,y2):ms) s
 | ((_status s) == Red) && ((x,y) `elem` (_redPieces s)) && not(((x2,y2) `elem` fRow))
  = setMessage $ set status Black $ apply_jump_move ((x2,y2):ms) $ set blackKings (removePiece (s^.blackKings) (mid_jump_coord (x,y)(x2,y2)) ) $ set blackPieces (removePiece (s^.blackPieces) (mid_jump_coord (x,y)(x2,y2)) ) $ set redPieces ((x2,y2):(removePiece (s^.redPieces) (x,y) )) s
 | ((_status s) == Red) && ((x,y) `elem` (_redPieces s)) && ((x2,y2) `elem` fRow)
  = setMessage $ set status Black $ apply_jump_move ((x2,y2):ms) $ set blackKings (removePiece (s^.blackKings) (mid_jump_coord (x,y)(x2,y2)) ) $ set blackPieces (removePiece (s^.blackPieces) (mid_jump_coord (x,y)(x2,y2)) ) $ set redPieces ((removePiece (s^.redPieces) (x,y) )) $ set redKings ((x2,y2):(removePiece (_redKings s) (x,y))) s
 | ((_status s) == Red) && ((x,y) `elem` (_redKings s))
  = setMessage $ set status Black $ apply_jump_move ((x2,y2):ms) $ set blackKings (removePiece (s^.blackKings) (mid_jump_coord (x,y)(x2,y2)) ) $ set blackPieces (removePiece (s^.blackPieces) (mid_jump_coord (x,y)(x2,y2)) ) $ set redKings ((x2,y2):(removePiece (s^.redKings) (x,y) )) s
 
 | ((_status s) == Black) && ((x,y) `elem` (_blackPieces s)) && not(((x2,y2) `elem` lRow))
  = setMessage $ set status Red $ apply_jump_move ((x2,y2):ms) $ set redKings (removePiece (s^.redKings) (mid_jump_coord (x,y)(x2,y2)) ) $ set redPieces (removePiece (s^.redPieces) (mid_jump_coord (x,y)(x2,y2)) ) $ set blackPieces ((x2,y2):(removePiece (s^.blackPieces) (x,y) )) s
 | ((_status s) == Black) && ((x,y) `elem` (_blackPieces s)) && ((x2,y2) `elem` lRow)
  = setMessage $ set status  Red $ apply_jump_move ((x2,y2):ms) $ set redKings (removePiece (s^.redKings) (mid_jump_coord (x,y)(x2,y2)) ) $ set redPieces (removePiece (s^.redPieces) (mid_jump_coord (x,y)(x2,y2)) ) $ set blackPieces ((removePiece (s^.blackPieces) (x,y) )) $ set blackKings ((x2,y2):(removePiece (_blackKings s) (x,y))) s
 | ((_status s) == Black) && ((x,y) `elem` (_blackKings s))
  = setMessage $ set status Red $ apply_jump_move ((x2,y2):ms) $ set redKings (removePiece (s^.redKings) (mid_jump_coord (x,y)(x2,y2)) ) $ set redPieces (removePiece (s^.redPieces) (mid_jump_coord (x,y)(x2,y2)) ) $ set blackKings ((x2,y2):(removePiece (s^.blackKings) (x,y) )) s
 |otherwise =  apply_jump_move [] s
{-
 | ((_status s) == Red) && ((x,y) `elem` (_redPieces s)) && not(((x2,y2) `elem` fRow))
  = apply_jump_move ((x2,y2):ms) $ s{_message = "Black turn" ,
                                     _status = Black,
                                     _blackKings = (removePiece (_blackKings s) (mid_jump_coord (x,y)(x2,y2)) ) ,
                                     _blackPieces = (removePiece (_blackPieces s) (mid_jump_coord (x,y)(x2,y2)) ) ,
                                     _redPieces = ((x2,y2):(removePiece (_redPieces s) (x,y) )) }
 | ((_status s) == Red) && ((x,y) `elem` (_redPieces s)) && ((x2,y2) `elem` fRow)
  = apply_jump_move ((x2,y2):ms) $  s{_message = "Black turn" ,
                                      _status = Black ,
                                      _blackKings = (removePiece (_blackKings s) (mid_jump_coord (x,y)(x2,y2)) ) ,
                                      _blackPieces = (removePiece (_blackPieces s) (mid_jump_coord (x,y)(x2,y2)) ) ,
                                      _redPieces = ((removePiece (_redPieces s) (x,y) )) ,
                                      _redKings = ((x2,y2):(removePiece (_redKings s) (x,y))) }
 | ((_status s) == Red) && ((x,y) `elem` (_redKings s))
  = apply_jump_move ((x2,y2):ms) $ s{_message = "Black turn" ,
                                     _status = Black , 
                                     _blackKings = (removePiece (_blackKings s) (mid_jump_coord (x,y)(x2,y2)) ) ,
                                     _blackPieces =(removePiece (_blackPieces s) (mid_jump_coord (x,y)(x2,y2)) ) ,
                                     _redKings = ((x2,y2):(removePiece (_redKings s) (x,y) )) }
 
 | ((_status s) == Black) && ((x,y) `elem` (_blackPieces s)) && not(((x2,y2) `elem` lRow))
  = apply_jump_move ((x2,y2):ms) $ s{_message = "Red turn" ,
                                     _status = Red ,
                                     _redKings = (removePiece (_redKings s) (mid_jump_coord (x,y)(x2,y2)) ) ,
                                     _redPieces = (removePiece (_redPieces s) (mid_jump_coord (x,y)(x2,y2)) ) ,
                                     _blackPieces = ((x2,y2):(removePiece (_blackPieces s) (x,y) )) }
 | ((_status s) == Black) && ((x,y) `elem` (_blackPieces s)) && ((x2,y2) `elem` lRow)
  = apply_jump_move ((x2,y2):ms) $ s{_message = "Red turn" ,
                                     _status =  Red ,
                                     _redKings =  (removePiece (_redKings s) (mid_jump_coord (x,y)(x2,y2)) ) ,
                                     _redPieces = (removePiece (_redPieces s) (mid_jump_coord (x,y)(x2,y2)) ) ,
                                     _blackPieces =  ((removePiece (_blackPieces s) (x,y) )) ,
                                     _blackKings = ((x2,y2):(removePiece (_blackKings s) (x,y))) }
 | ((_status s) == Black) && ((x,y) `elem` (_blackKings s))
  = apply_jump_move ((x2,y2):ms) $ s{_message = "Red turn" ,
                                     _status = Red ,
                                     _redKings = (removePiece (_redKings s) (mid_jump_coord (x,y)(x2,y2)) ) ,
                                     _redPieces = (removePiece (_redPieces s) (mid_jump_coord (x,y)(x2,y2)) ) ,
                                     _blackKings = ((x2,y2):(removePiece (_blackKings s) (x,y) )) }
 |otherwise =  apply_jump_move [] s
---------------------------------------
-}
toogle_status::GameState->GameState
toogle_status s
 |((not(is_there (_redPieces s))) && (not(is_there (_redKings s)))) || (not(is_there (moves s))) = s{_status = GameOver}
                                                                                                     
 |((not(is_there (_blackPieces s))) && (not(is_there (_blackKings s)))) || (not(is_there (moves s))) = s{_status = GameOver}
                                                                                                         
 | otherwise = s




fRow = [(0,0), (1,0),(2,0), (3,0),(4,0) ,(5,0),(6,0), (7,0)]
lRow  = [ (0,7), (1,7),(2,7),(3,7), (4,7), (5,7),(6,7), (7,7)]


