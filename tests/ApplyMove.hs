{-# LANGUAGE OverloadedStrings, TemplateHaskell, FlexibleContexts #-}
module ApplyMove where

import GameLogic
import Data.List
import Moves
-------------------------- "My code line"

--takes in a move and a gamestate and outputs the resulting gamestate
apply_move :: Move -> GameState ->  GameState
apply_move [] (GameState bp rp bk rk s m)
  |(moves (GameState bp rp bk rk s m) == []) = (GameState bp rp bk rk GameOver m) --gameover
  |otherwise = (GameState bp rp bk rk s m) -- no movement
apply_move (x:[]) (GameState bp rp bk rk s m) --only 1 thing, no movement
  |(moves (GameState bp rp bk rk s m) == []) = (GameState bp rp bk rk GameOver m) --gameover
  |otherwise = (GameState bp rp bk rk s m) -- no movement
apply_move ((x,y):(x2,y2):ms) (GameState bp rp bk rk s m)
  |(moves (GameState bp rp bk rk s m) == []) = (GameState bp rp bk rk GameOver m) --gameover
  | ((inlist ((x,y):(x2,y2):ms) (moves (GameState bp rp bk rk s m)) ) == False) = (GameState bp rp bk rk s m) --must be valid, otherwise do nothing
  | (((abs (x-x2)) == 1) && ((abs (y-y2)) == 1)) = applySimple ((x,y):(x2,y2):ms) (GameState bp rp bk rk s m) --simple moves, x and y must only be 1 away
  | otherwise = applyJump ((x,y):(x2,y2):ms) (GameState bp rp bk rk s m)

--applies a jump move given a gamestate and a move
applyJump :: Move -> GameState -> GameState
applyJump x (GameState bp rp bk rk s m)
  | (s == Red && (inlist (x!!0) rp)) = applyJumpredP x (x!!0) (GameState bp rp bk rk s m) False
  | (s == Red && (inlist (x!!0) rk)) = applyJumpredK x (x!!0) (GameState bp rp bk rk s m)
  | (s == Black && (inlist (x!!0) bp)) = applyJumpblackP x (x!!0) (GameState bp rp bk rk s m) False
  | otherwise = applyJumpblackK x (x!!0) (GameState bp rp bk rk s m)

--applies a red piece jump move 
applyJumpredP :: Move -> Coord -> GameState -> Bool -> GameState
applyJumpredP [] st (GameState bp rp bk rk s m) pstat = (GameState bp rp bk rk s m) -- should never execute
applyJumpredP (x:[]) st (GameState bp rp bk rk s m) pstat
  |(pstat == False) = (GameState bp ((filter (\y -> y /= st) (rp))++[x]) bk rk Black m) -- case where it was not promoted
  | otherwise = (GameState bp (filter (\y -> y /= st) (rp)) bk (rk ++ [x]) Black m) -- case where it is
applyJumpredP ((x,y):(x2,y2):ms) st (GameState bp rp bk rk s m) pstat
  |(inlist (mid) bp) && (y2 == 0) = applyJumpredP ((x2,y2):ms) st (GameState (filter  (\y -> y /= mid) bp) rp bk rk s m) True
  |(inlist (mid) bp) && (y2 /= 0) = applyJumpredP ((x2,y2):ms) st (GameState (filter  (\y -> y /= mid) bp) rp bk rk s m) pstat
  |(inlist (mid) bk) && (y2 == 0) = applyJumpredP ((x2,y2):ms) st (GameState bp rp (filter  (\y -> y /= mid) bk) rk s m) True
  |(inlist (mid) bk) && (y2 /= 0) = applyJumpredP ((x2,y2):ms) st (GameState bp rp (filter  (\y -> y /= mid) bk) rk s m) pstat
    where
      mid = ((quot (x+x2) 2),(quot (y+y2) 2))

--applies a black piece jump move
applyJumpblackP :: Move -> Coord -> GameState -> Bool -> GameState
applyJumpblackP [] st (GameState bp rp bk rk s m) pstat = (GameState bp rp bk rk s m) -- should never execute
applyJumpblackP (x:[]) st (GameState bp rp bk rk s m) pstat
  |(pstat == False) = (GameState ((filter (\y -> y /= st) (bp))++[x]) rp bk rk Red m) -- case where it was not promoted
  | otherwise = (GameState (filter (\y -> y /= st) (bp)) rp (bk ++ [x]) rk Red m) -- case where it is
applyJumpblackP ((x,y):(x2,y2):ms) st (GameState bp rp bk rk s m) pstat
  |(inlist (mid) rp) && (y2 == 7) = applyJumpblackP ((x2,y2):ms) st (GameState bp (filter (\y -> y /=mid )rp) bk rk s m) True
  |(inlist (mid) rp) && (y2 /= 7) = applyJumpblackP ((x2,y2):ms) st (GameState bp (filter (\y -> y /=mid )rp) bk rk s m) pstat
  |(inlist (mid) rk) && (y2 == 7) = applyJumpblackP ((x2,y2):ms) st (GameState bp rp bk (filter (\y -> y /=mid )rk) s m) True
  |(inlist (mid) rk) && (y2 /= 7) = applyJumpblackP ((x2,y2):ms) st (GameState bp rp bk (filter (\y -> y /=mid )rk) s m) pstat
    where
      mid = ((quot (x+x2) 2),(quot (y+y2) 2))

--applies a jump red king move
applyJumpredK:: Move -> Coord -> GameState -> GameState
applyJumpredK [] st (GameState bp rp bk rk s m) = (GameState bp rp bk rk s m) -- should never execute
applyJumpredK (x:[]) st (GameState bp rp bk rk s m) = (GameState bp rp bk ((filter (\y -> y /= st) (rk))++[x]) Black m)
applyJumpredK ((x,y):(x2,y2):ms) st (GameState bp rp bk rk s m)
  |(inlist (mid) bp) = applyJumpredK ((x2,y2):ms) st (GameState (filter  (\y -> y /= mid) bp) rp bk rk s m)
  |(inlist (mid) bk) = applyJumpredK ((x2,y2):ms) st (GameState bp rp (filter  (\y -> y /= mid) bk) rk s m)
    where
      mid = ((quot (x+x2) 2),(quot (y+y2) 2))

--applies a jump black king move
applyJumpblackK:: Move -> Coord -> GameState -> GameState
applyJumpblackK [] st (GameState bp rp bk rk s m) = (GameState bp rp bk rk s m) -- should never execute
applyJumpblackK (x:[]) st (GameState bp rp bk rk s m) = (GameState bp rp ((filter (\y -> y /= st) (bk))++[x]) rk Red m)
applyJumpblackK ((x,y):(x2,y2):ms) st (GameState bp rp bk rk s m)
  |(inlist (mid) rp) = applyJumpblackK ((x2,y2):ms) st (GameState bp (filter  (\y -> y /= mid) rp) bk rk s m)
  |(inlist (mid) rk) = applyJumpblackK ((x2,y2):ms) st (GameState bp rp bk (filter  (\y -> y /= mid) rk) s m)
    where
      mid = ((quot (x+x2) 2),(quot (y+y2) 2))

--applies a simple move
applySimple :: Move -> GameState -> GameState
applySimple x (GameState bp rp bk rk s m)
  | (s == Red && (inlist (x!!0) rp) && (snd(x!!1) /= 0)) = (GameState bp (filter (\y -> y /= x!!0) (rp ++ [x!!1])) bk rk Black m) --red piece simple
  | (s == Red && (inlist (x!!0) rp) && (snd(x!!1) == 0)) = (GameState bp (filter (\y -> y /= x!!0) rp) bk (rk ++ [x!!1]) Black m) --red piece simple Promotion
  | (s == Red && (inlist (x!!0) rk)) = (GameState bp rp bk (filter (\y -> y /= x!!0) (rk ++ [x!!1])) Black m) -- red king simple
  | (s == Black && (inlist (x!!0) bp) && (snd(x!!1) /= 7)) = (GameState (filter (\y -> y /= x!!0) (bp ++ [x!!1])) rp bk rk Red m) --black piece simple
  | (s == Black && (inlist (x!!0) bp) && (snd(x!!1) == 7)) = (GameState (filter (\y -> y /= x!!0) bp) rp (bk ++ [x!!1]) rk Red m) --black piece simple Promotion
  | otherwise = (GameState bp rp (filter (\y -> y /= x!!0) (bk ++ [x!!1])) rk Red m) -- black king simple

-----------------------------------