module Moves where

import Checkers.Types

-- NOTE: I used Robin's pseudocode from the course website to get an idea of how to do some of the list comprehensions and set up a few of the functions
-- general structure throughout the code.

-- This function is our "main" function of this file. It calls simple_moves and jump_moves to get all the valid simple and jump moves available to the 
-- current player. We will use this to determine if the move the player makes is valid or not.
moves :: GameState -> ([Move],[Move])
moves g = (simple_moves g, jump_moves g)

-- Gets all the simple moves for the current player whose turn it is. We call simpleKing and simplePiece to get all the simple moves for normal pieces
-- and kings, and then append these two lists together to get all the simple moves. If it is neither players move, then the game must be over and thus
-- we return the empty list as there are no moves.
simple_moves :: GameState -> [Move]
simple_moves g
    | _status g == Red = (simpleKing (redKings g) g) ++ (simplePiece (redPieces g) g)
    | _status g == Black = (simpleKing (blackKings g) g) ++ (simplePiece (blackPieces g) g)
    | otherwise = []

-- This function takes a GameState and returns the Player whose turn it is, either Red or Black. We use status from GameState to get whose Turn it is,
-- and if its Turn Red, we return Red. Otherwise its Blacks turn so we return Black.
_status :: GameState -> Player
_status g
    | status g == Turn Red = Red
    | otherwise = Black

-- This function gets all the valid simple moves for kings that are passed to it. We use a list comprehension. We take the coord from the list
-- that was given to us as (x, y) and then get the four squares that the king can possibly move to. We make sure the move won't cause a repeated gamestate, 
-- and also check the move is on the board and isnt already occupied. If the move is valid, it gets added to the list, so at the end we have all the possible 
-- valid simple moves for each king.
simpleKing :: [Coord] -> GameState -> [[PorK Coord]]
simpleKing xs g = [ [K (x,y), K (x',y')] | (x,y) <- xs, (x',y') <- [(x+1,y+1), (x-1,y+1), (x+1,y-1), (x-1,y-1)]
                  , notoccupied (x', y') g && onboard (x', y') && check_history ([[K (x,y), K (x', y')]] ++ history g) [] 0 ]

-- This function checks the history of our game to make sure a GameState is not repeated if the move is made. We take history with the potential move, an empty
-- list (movement), and an Int (represents where in the function we want to go). We first check when the Int is 1 or 2 and return True in this situation to follow the
-- algorithm. The next line, we put the first two simple king moves (reds first move and blacks first move) into our movement list and then call the function
-- with Int = 1. This tells us that we have added the moves into movement. Our lines with 1 and 2 check the next move in history, and acts accordingly based on 
-- what things are equal. The lines with 3 and 4 do the exact same thing as the lines with 1 and 2, but if they are found to be "equal" moves, we return false
-- as the GameState is repeated. Our final line just is a final check if every other test was not pattern matched, then our history must have only had 1 move 
-- or the history had a non simple king move, and therefore we return True. So, we can see that if we arrive at a case where history is empty with Int = 1 or 2
-- and movement is not empty, no repeated state took place, and we return True. The general idea of this algorithm came from Alex's powerpoint from Tut10.
check_history :: [Move] -> [Move] -> Int -> Bool
check_history [] _ 1 = True
check_history [] _ 2 = True
check_history ([x,y]:[x',y']:xs) _ 0 = check_history xs [[x,y], [x',y']] 1
check_history ([n1, y]:xs1) ([x, n2]:xs2) 1
    | n1 == n2 = if (x == y) then check_history xs1 xs2 3 else check_history xs1 ([x, y]:xs2) 2
    | otherwise = check_history xs1 ([x, n2]:xs2) 2
check_history ([n1,y]:xs1) (f:[x,n2]:xs2) 2
    | n1 == n2 = if (x == y) then check_history xs1 (f:xs2) 4 else check_history xs1 (f:[x, y]:xs2) 1 
    | otherwise = check_history xs1 (f:[x,n2]:xs2) 1
check_history ([n1,y]:xs1) ([x,n2]:xs2) 3
    | n1 == n2 = if (x == y) then False else check_history xs1 ([n1,y]:[x, y]:xs2) 2
    | otherwise = check_history xs1 ([n1,y]:[x,n2]:xs2) 2
check_history ([n1,y]:xs1) ([x,n2]:xs2) 4
    | n1 == n2 = if (x == y) then False else check_history xs1 ([x, y]:[n1,y]:xs2) 1
    | otherwise = check_history xs1 ([x,n2]:[n1,y]:xs2) 1
check_history _ _ _ = True

-- This function gets all the valid simple moves for normal pieces that are passed to it. We use a list comprehension. We take the coord from the list
-- that was given to us as (x', y') and based on whose turn it is we get (x', y') from another list that represents the two squares where the piece can
-- move. We check the coordinates that the piece "might" move to by checking that it is on the board and that a piece is not already occupying it. Finally,
-- if the coords pass all these tests, then we also check if the square that the peiece is moving to is with y' == 0 or 7, and if it is we change the 
-- piece to a King.
simplePiece :: [Coord] -> GameState -> [[PorK Coord]]
simplePiece xs g = [ [P (x,y), (if (y' == 0 || y' == 7) then K else P) (x',y')] | (x,y) <- xs, (x',y') <- let y' = y + dir g in [(x+1,y'), (x-1,y')], notoccupied (x', y') g && onboard (x', y')]

-- Takes a GameState and returns an Int, either 1 or -1 which is the direction on the board we are going. If we are Red, our y value is decreasing as we
-- move up the board, so we return -1. Otherwise we are Black, and we are increasing our y so we return 1.
dir :: GameState -> Int
dir g
    | _status g == Red = -1
    | otherwise = 1

-- This function determines if a set of coordinates is on the gameboard. For it to be on the board, x and y have to be between 0 and 7 inclusive. So,
-- we check for this conditiona and if it is true, we return True. Otherwise the coords are not on the board and we return False.
onboard :: (Int, Int) -> Bool
onboard (x, y)
    | x >= 0 && x <= 7 && y >= 0 && y <= 7 = True
    | otherwise = False

-- This function determines if a coodrinate on the baord is occupied by a piece or not. We use our elem' function to check all the pieces on the board,
-- and if the coord is in any of these lists, we return False, as the square is occupied. Otherwise, the square is not occupied and we return True.
notoccupied :: (Int, Int) -> GameState -> Bool
notoccupied x g
    | x `elem'` (redPieces g) || x `elem'` (redKings g) || x `elem'` (blackPieces g) || x `elem'` (blackKings g) = False
    | otherwise = True

-- This function determines if a given element is in a list. If the list is empty, we return False. Then, we use pattern matching to get the first
-- element of the list and compare to our given element. If they are equal, we return True, and otherwise we call our function recursively with the
-- given element and the rest of the list. So, either we find an equal element or our list eventually becomes empty and we return False.
elem' :: Eq a => a -> [a] -> Bool
elem' x [] = False
elem' x (y:ys)
    | x == y = True
    | otherwise = elem' x ys

-- Gets all the jump moves for the current player whose turn it is. We call jumpKing and jumpPiece to get all the jump moves for normal pieces
-- and kings, and then append these two lists together to get all the jump moves. If it is neither players move, then the game must be over and thus
-- we return the empty list as there are no moves.
jump_moves :: GameState -> [Move]
jump_moves g
    | _status g == Red = (jumpKing (redKings g) g) ++ (jumpPiece (redPieces g) g)
    | _status g == Black = (jumpKing (blackKings g) g) ++ (jumpPiece (blackPieces g) g)
    | otherwise = []

-- This function goes through each king and finds the jump moves for it, if there are any. We use a list comprehension to get each king, and use our
-- jumpKing' function to find any jump moves.
jumpKing :: [Coord] -> GameState -> [[PorK Coord]]
jumpKing xs g = [K (x,y):ys | (x,y) <- xs, ys <- jumpKing' (x,y) [] (x,y) g]

-- This function uses list comprehension and recursion to find jump moves (or multiple jump moves) for a king. We get all the squares where the king could
-- jump to, and then make sure the opponent is occupying the square they are jumping, we arent jumping our own piece, we are on the board, the space we are
-- jumping to is empty. Also, if the place we are jumping to is where we started, the not occupied will be false, so that why we have || in this case. We
-- do this recursively, because in checkers we can make multiple jumps on one move, so we need to take this into consideration.
jumpKing' :: (Int, Int) -> [Coord] -> (Int, Int) -> GameState -> [[PorK Coord]]
jumpKing' start rem (x,y) g =
                   [ K (x'',y''):ys
                   | ((x',y'),(x'',y'')) <- [((x+1,y+1),(x+2,y+2)),((x-1,y+1),(x-2,y+2)),((x+1,y-1),(x+2,y-2)),((x-1,y-1),(x-2,y-2))]
                   , not ((x',y') `elem'` rem) && opponent_occupied (x',y') g && (start == (x'',y'') || notoccupied (x'',y'') g && onboard (x'',y''))
                   , ys <- jump_over (jumpKing' start ((x',y'):rem) (x'',y'') g) ]

-- When the jumps are over, we must return a list containing the empty list in order to smoothly terminate the recursive calling of the jumps. So, this
-- is the purpose of this function. If the list is still not empty we just return it without changing it, but if it is empty, we return a list with an
-- empty list as its only element.
jump_over :: [[PorK Coord]] -> [[PorK Coord]]
jump_over [] = [[]]
jump_over z = z

-- This function determines if the opponent is occupying the specified square. We find whoes turn it is, and using that we look through the opposite 
-- pieces and check if our coord is in the opponents pieces using elem'. If it is we return True, and if it isn't we return False.
opponent_occupied :: (Int, Int) -> GameState -> Bool
opponent_occupied x g
    | _status g == Red = if x `elem'` (blackKings g ++ blackPieces g) then True else False
    | _status g == Black = if x `elem'` (redKings g ++ redPieces g) then True else False

-- This function goes through each normal piece and finds the jump moves for it, if there are any. We use a list comprehension to get each piece, and use our
-- jumpPiece' function to find any jump moves.
jumpPiece :: [Coord] -> GameState -> [[PorK Coord]]
jumpPiece xs g = [P (x,y):ys | (x,y) <- xs, ys <- jumpPiece' (x,y) [] (x,y) g]

-- This function uses list comprehension and recursion to find jump moves (or multiple jump moves) for a normal piece. We get all the squares where the piece 
-- could jump to, and then make sure the opponent is occupying the square they are jumping, we arent jumping our own piece, we are on the board, the space we 
-- are jumping to is empty. Also, if the place we are jumping to is where we started, the not occupied will be false, so that why we have || in this case. We
-- do this recursively, because in checkers we can make multiple jumps on one move, so we need to take this into consideration. Also, since in our variation 
-- a normal piece can become a king and keep jumping, we check if the piece became a king, and if it did we call jumpKing' instead of jumpPiece' recursively.
-- We also check this property for the PorK datatype so that if we became a king, we have K instead of P.
jumpPiece' :: (Int, Int) -> [Coord] -> (Int, Int) -> GameState -> [[PorK Coord]]
jumpPiece' start rem (x,y) g =
                    [ (if (y'' == 0 || y'' == 7) then K else P) (x'',y''):ys
                    | ((x',y'),(x'',y'')) <- let y' = y + dir g
                                                 y'' = y + dir2 g 
                                                 in [((x+1,y'),(x+2,y'')),((x-1,y'),(x-2,y''))]
                    , not ((x',y') `elem'` rem) && opponent_occupied (x',y') g && (start == (x'',y'') || notoccupied (x'',y'') g && onboard (x'',y''))
                    , ys <- jump_over (if (y'' == 0 || y'' == 7) then jumpKing' start ((x',y'):rem) (x'',y'') g else jumpPiece' start ((x',y'):rem) (x'',y'') g) ]

-- Takes a GameState and returns an Int, either 2 or -2 which is the direction on the board we are going. If we are Red, our y value is decreasing as we
-- move up the board, so we return -2. Otherwise we are Black, and we are increasing our y so we return 2.
dir2 :: GameState -> Int
dir2 g
    | _status g == Red = -2
    | otherwise = 2


data RorB a = R a | B a
  deriving (Show,Eq, Read)

repeatedState :: [Move] -> GameState -> Bool
repeatedState ([K (x,y), K (x2,y2)]:xs) st -- first move in history is already known to be 
    | status st == RedPlayer = repeatedState2 [R (x2,y2)] [R (x,y)] 2 xs
    | status st == BlackPlayer = repeatedState2 [B (x2,y2)] [B (x,y)] 1 xs

repeatedState2 :: [RorB Coord] -> [RorB Coord] -> Int -> [Move] -> Bool
repeatedState2 [] _ _ _ = True -- repeated state
repeatedState2 vacant moving 1 ([K (x,y), K (x2,y2)]:xs) -- need to check for jump moves as well somehow
    | elem (R (x2,y2)) moving && elem (R (x,y)) vacant = repeatedState2 (removeMove (R (x,y)) vacant) (removeMove (R (x2,y2)) moving) 2 xs
    | elem (R (x2,y2)) moving && not (elem (R (x,y) ) vacant) = repeatedState2 vacant (replaceMove (R (x2,y2)) (R (x,y)) moving) 2 xs
    | not (elem (R (x2,y2)) moving) = repeatedState2 ((R (x2,y2)):vacant) ((R (x,y)):moving) 2 xs
repeatedState2 vacant moving 2 ([K (x,y), K (x2,y2)]:xs)
    | elem (B (x2,y2)) moving && elem (B (x,y)) vacant = repeatedState2 (removeMove (B (x,y)) vacant) (removeMove (B (x2,y2)) moving) 1 xs
    | elem (B (x2,y2)) moving && not (elem  (B (x,y)) vacant) = repeatedState2 vacant (replaceMove (B (x2,y2)) (B (x,y)) moving) 1 xs
    | not (elem (B (x2,y2)) moving ) = repeatedState2 ((B (x2,y2)):vacant) ((B (x,y)):moving) 1 xs
repeatedState2 _ _ _ _ = False