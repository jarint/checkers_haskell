module Moves where

import GameLogic
import Data.List
-----------------------------------
--helper method to determine if an element is in a list
inlist :: (Eq a) => a -> [a] -> Bool
inlist x [] = False
inlist x (y:ys)
    |x == y = True
    |otherwise = inlist x ys

--take a coord and returns "R" if red, "B" if black, "RK" if redKing "BK" if blackKing, "off" if out of bounds
occupied :: Coord -> GameState -> String
occupied (x,y) (GameState bp rp bk rk s m)
    |inlist (x,y) bp = "B" 
    |inlist (x,y) bk = "BK"
    |inlist (x,y) rp = "R"
    |inlist (x,y) rk = "RK"
    |(x > 7 || x < 0 || y > 7 || y < 0) = "off"         --out of bounds
    |otherwise = "$"

--takes in coordinate and returns possible moves (even if it is invalid) 
simplePossible :: Coord -> GameState -> [Move]
simplePossible (x,y) (GameState bp rp bk rk s m)
      | inlist (x,y) bp = [[(x,y),(x-1,y+1)],[(x,y),(x+1,y+1)]]                                  --if the coordinate passed is a black piece.
      | inlist (x,y) rp = [[(x,y),(x-1,y-1)],[(x,y),(x+1,y-1)]]                                  --if coordinate passed in red piece
      | (inlist (x,y) bk || inlist (x,y) rk) = [[(x,y),(x-1,y-1)],[(x,y),(x-1,y+1)],[(x,y),(x+1,y-1)],[(x,y),(x+1,y+1)]] -- if coordinate passed is a king

--takes in a list of moves and filters out illegal ones
simpleFilter :: [Move] -> GameState -> [Move]
simpleFilter [] (GameState bp rp bk rk s m) = []
simpleFilter (x:xs) (GameState bp rp bk rk s m)
    |occupied (x!!1) (GameState bp rp bk rk s m) == "$" = [x] ++ simpleFilter xs (GameState bp rp bk rk s m)
    |otherwise = simpleFilter xs (GameState bp rp bk rk s m)

--finds simple moves given a board state  (ADD case for GAMEOVER)                  
simple_moves::GameState -> [Move]
simple_moves (GameState bp rp bk rk s m)
    | s == Red = simpleredP (GameState bp rp bk rk s m) rp ++ simpleredK (GameState bp rp bk rk s m) rk --returns simple moves for red
    | otherwise = simpleblackP (GameState bp rp bk rk s m) bp ++ simpleblackK (GameState bp rp bk rk s m) bk -- otherwise returns for black

--takes a gamestate and returns all potential moves BIG ONE    |a move will consist of [(current coord),(potential coords)....]
moves:: GameState -> [Move]
moves (GameState bp rp bk rk s m)
      |(jump_moves (GameState bp rp bk rk s m) /= []) = jump_moves (GameState bp rp bk rk s m)      --if there is a jump move available, then it must be chosen
      |otherwise = simple_moves (GameState bp rp bk rk s m)                                         --otherwise choose simple

--returns all simple moves for red pieces
simpleredP :: GameState -> [Coord] -> [Move]
simpleredP g [] = []
simpleredP g (r:rp) = (simpleFilter (simplePossible r g) g) ++ simpleredP g rp

--returns all simple moves for black pieces
simpleblackP :: GameState -> [Coord] -> [Move]
simpleblackP g [] = []
simpleblackP g (b:bp) = (simpleFilter (simplePossible b g) g) ++ simpleblackP g bp

--returns all simple moves for red kings
simpleredK :: GameState -> [Coord] -> [Move]
simpleredK g [] = []
simpleredK g (r:rk) = simpleFilter (simplePossible r g) g ++ simpleredK g rk

--returns all simple moves for black kings
simpleblackK :: GameState -> [Coord] -> [Move]
simpleblackK g [] = []
simpleblackK g (b:bk) = simpleFilter (simplePossible b g) g ++ simpleblackK g bk

--returns all jump moves given a gamestate
jump_moves :: GameState -> [Move]
jump_moves (GameState bp rp bk rk s m)
        | s == Red = filtersubmoves (reverse (filtersubmoves(jumpred (GameState bp rp bk rk s m) rp))) ++ filtersubmoves(reverse (filtersubmoves(jumpredK (GameState bp rp bk rk s m) rk)))
        | otherwise = filtersubmoves(reverse (filtersubmoves(jumpblack (GameState bp rp bk rk s m) bp))) ++ filtersubmoves(reverse (filtersubmoves(jumpblackK (GameState bp rp bk rk s m) bk)))

--finds the list of jumpmoves given a gamestate and list of coords
jumpred :: GameState -> [Coord] -> [Move]
jumpred g [] = []
jumpred g (r:rp) = filter ((>1).length) (singlejumpred g r (True) [] r  ++ jumpred g rp) 
        where
          singlejumpred:: GameState -> Coord -> Bool -> [Coord] -> Coord -> [Move]
          singlejumpred g (x,y) isValid pjump s
            | (isValid == False) = [[]]
            | (upleft || upright) = map ((x,y):) ((singlejumpred g (x-2,y-2) upleft ((x-1,y-1):pjump) s) ++ (singlejumpred g (x+2,y-2) upright ((x+1,y-1):pjump) s)) --recursive case
            | y == 0 = singlejumpredK g (x,y) True pjump s --case where the piece hits the end and becomes a king
            | otherwise = [[(x,y)]] --base case
              where
                upleft = (occupied (x-2,y-2) g == "$" && ((occupied (x-1,y-1) g == "B") || (occupied (x-1,y-1) g == "BK")) || ((x-2,y-2) == s && ((length pjump) > 1))) && (inlist (x-1,y-1) pjump == False) 
                upright = (occupied (x+2,y-2) g == "$" && ((occupied (x+1,y-1) g == "B") || (occupied (x+1,y-1) g == "BK")) || ((x+2,y-2) == s && ((length pjump) > 1))) && (inlist (x+1,y-1) pjump == False)          
                
--finds the list of jumpmoves given a gamestate and list of coords
jumpblack :: GameState -> [Coord] -> [Move]
jumpblack g [] = []
jumpblack g (b:bp) = filter ((>1).length) (singlejumpblack g b (True) [] b ++ jumpblack g bp)
        where
          singlejumpblack:: GameState -> Coord -> Bool -> [Coord] -> Coord -> [Move]
          singlejumpblack g (x,y) isValid pjump s
            | (isValid == False) = [[]] --if invalid then empty
            | (downleft || downright) = map ((x,y):) ((singlejumpblack g (x-2,y+2) downleft ((x-1,y+1):pjump) s) ++ (singlejumpblack g (x+2,y+2) downright ((x+1,y+1):pjump) s)) --recursive case
            | y == 7 = singlejumpblackK g (x,y) True pjump s --case where piece hits the end and becomes a king
            | otherwise = [[(x,y)]] --base case
              where
                downleft = (occupied (x-2,y+2) g == "$" && ((occupied (x-1,y+1) g == "R") || (occupied (x-1,y+1) g == "RK")) || ((x-2,y+2) == s && ((length pjump) > 1))) && (inlist (x-1,y+1) pjump == False)
                downright = (occupied (x+2,y+2) g == "$" && ((occupied (x+1,y+1) g == "R") || (occupied (x+1,y+1) g == "RK")) || ((x+2,y+2) == s && ((length pjump) > 1)))  && (inlist (x+1,y+1) pjump == False)
          
--finds jump moves for red kings
jumpredK :: GameState -> [Coord] -> [Move]
jumpredK g [] = []
jumpredK g (r:rk) = filter ((>1).length) (singlejumpredK g r (True) [] r) ++ (jumpredK g rk)

singlejumpredK :: GameState -> Coord -> Bool -> [Coord] -> Coord -> [Move]
singlejumpredK g (x,y) isValid pjump s
  | (isValid == False) = [[]] --if invalid then empty
  | (downleft || downright || upleft || upright) = map ((x,y):) ((singlejumpredK g (x-2,y-2) upleft ((x-1,y-1):pjump) s) ++ (singlejumpredK g (x+2,y-2) upright ((x+1,y-1):pjump) s) ++ (singlejumpredK g (x-2,y+2) downleft ((x-1,y+1):pjump) s) ++ (singlejumpredK g (x+2,y+2) downright ((x+1,y+1):pjump) s))
  | otherwise = [[(x,y)]] --base case
    where
      downleft = (occupied (x-2,y+2) g == "$" || ((x-2,y+2) == s && ((length pjump) > 1))) && ((occupied (x-1,y+1) g == "B") || (occupied (x-1,y+1) g == "BK")) && (inlist (x-1,y+1) pjump == False)
      downright = (occupied (x+2,y+2) g == "$" || ((x+2,y+2) == s && ((length pjump) > 1))) && ((occupied (x+1,y+1) g == "B") || (occupied (x+1,y+1) g == "BK"))  && (inlist (x+1,y+1) pjump == False)
      upleft = (occupied (x-2,y-2) g == "$" || ((x-2,y-2) == s && ((length pjump) > 1))) && ((occupied (x-1,y-1) g == "B") || (occupied (x-1,y-1) g == "BK")) && (inlist (x-1,y-1) pjump == False) 
      upright = (occupied (x+2,y-2) g == "$" || ((x+2,y-2) == s && ((length pjump) > 1))) && ((occupied (x+1,y-1) g == "B") || (occupied (x+1,y-1) g == "BK")) && (inlist (x+1,y-1) pjump == False)

--finds jump moves for black kings
jumpblackK :: GameState -> [Coord] -> [Move]
jumpblackK g [] = []
jumpblackK g (b:bk) = filter ((>1).length) (singlejumpblackK g b (True) [] b) ++ (jumpblackK g bk)

singlejumpblackK :: GameState -> Coord -> Bool -> [Coord] -> Coord -> [Move]
singlejumpblackK g (x,y) isValid pjump s
  | (isValid == False) = [[]]
  | (downleft || downright || upleft || upright) = map ((x,y):) ((singlejumpblackK g (x-2,y-2) upleft ((x-1,y-1):pjump) s) ++ (singlejumpblackK g (x+2,y-2) upright ((x+1,y-1):pjump) s) ++ (singlejumpblackK g (x-2,y+2) downleft ((x-1,y+1):pjump) s) ++ (singlejumpblackK g (x+2,y+2) downright ((x+1,y+1):pjump) s))
  | otherwise = [[(x,y)]] --base case  
  where
      downleft = (occupied (x-2,y+2) g == "$" || ((x-2,y+2) == s && ((length pjump) > 1))) && ((occupied (x-1,y+1) g == "R") || (occupied (x-1,y+1) g == "RK")) && (inlist (x-1,y+1) pjump == False)
      downright = (occupied (x+2,y+2) g == "$" || ((x+2,y+2) == s && ((length pjump) > 1))) && ((occupied (x+1,y+1) g == "R") || (occupied (x+1,y+1) g == "RK"))  && (inlist (x+1,y+1) pjump == False)
      upleft = (occupied (x-2,y-2) g == "$"  || ((x-2,y-2) == s && ((length pjump) > 1))) && ((occupied (x-1,y-1) g == "R") || (occupied (x-1,y-1) g == "RK")) && (inlist (x-1,y-1) pjump == False) 
      upright = (occupied (x+2,y-2) g == "$"  || ((x+2,y-2) == s && ((length pjump) > 1))) && ((occupied (x+1,y-1) g == "R") || (occupied (x+1,y-1) g == "RK")) && (inlist (x+1,y-1) pjump == False)

--filters "submoves" of another in the list
filtersubmoves :: [Move] -> [Move]
filtersubmoves [] = []
filtersubmoves (x:xs)
    |filtersubWork x (xs) == True = filtersubmoves xs
    |otherwise = x : (filtersubmoves xs)
                where
                  filtersubWork :: Move -> [Move] -> Bool
                  filtersubWork a [] = False
                  filtersubWork a (b:bs)
                    | (isPrefixOf a b) == False = filtersubWork a bs
                    | otherwise = True

