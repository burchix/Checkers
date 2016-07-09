----------------------MOVES----------
module Moves where

import Board
import Data.List
import Data.Function

plus, minus :: Pos -> Pos -> Pos
plus (x1,y1) (x2,y2) = (x1+x2, y1+y2)
minus (x1,y1) (x2,y2) = (x1-x2, y1-y2)

prod :: Pos -> Int -> Pos
prod (x,y) a = (x*a,y*a)

blackM,whiteM,captM :: [Pos]
whiteM = [(-1, 1), (1,1)]
blackM = [(-1, -1), (1,-1)]
captM = [(-1, -1), (-1, 1), (1,-1), (1,1)]

--spr czy na innym polu jest przeciwnik
oposite, friend :: Square -> Square -> Bool
oposite x y | x `elem` [White, WhiteKing] && y `elem`  [Black, BlackKing] = True
            | y `elem` [White, WhiteKing] && x `elem`  [Black, BlackKing] = True
            | otherwise = False
friend x y | x `elem` [White, WhiteKing] && y `elem` [White, WhiteKing] = True
           | y `elem` [Black, BlackKing] && x `elem` [Black, BlackKing] = True
           | otherwise = False
         
reverseField :: Square -> Square
reverseField checker = case checker of
                        White -> Black
                        WhiteKing -> Black                        
                        Black -> White
                        BlackKing -> White
            
isInside,isLegal :: Pos -> Bool
isInside (x,y) = x `elem` [1..8] &&  y `elem` [1..8]
isLegal (x,y) = isInside (x,y) && (even $ x + y)

isEmpty, isTaken :: Board -> Pos -> Bool
isEmpty board src = Empty == takeC board src
isTaken board src = not $ isEmpty board src

----------------------Ktory ruch ma najwiecej krokow (bije najwiecej innych)--------
bestMove :: [[Pos]] -> [[Pos]]
bestMove x | x == [] = []
           | otherwise = last . (groupBy ((==) `on` length)) . (sortBy (compare `on` length)) $ x


move :: Board -> Pos -> [[Pos]]
move b src | checker == White || checker == Black = moveMen b src
           | checker == WhiteKing || checker == BlackKing = moveKing b src
           where checker = takeC b src
           
isCapture :: Board -> Pos -> Bool
isCapture board src | checker == White || checker == Black = posibleCapt board src /= []
                    | checker == WhiteKing || checker == BlackKing = posibleCaptKing board src /= []
                    where checker = takeC board src
                    
--------------------------Ruch Kamienia---------------------
moveMen :: Board -> Pos -> [[Pos]]
moveMen board src =if isCapture board src then bestMove $ captMoveMen board src else noCaptMoveMen board src

noCaptMoveMen :: Board -> Pos -> [[Pos]]
noCaptMoveMen board src = map (flip (:) []) . filter (isEmpty board) . filter (isLegal) $ map (plus src) $ 
                    if takeC board src == Black then blackM else whiteM 

captMoveMen :: Board -> Pos -> [[Pos]]
captMoveMen board src = concatMap magicFun (posibleCapt board src)
                        where 
                        magicFun = (\[dst] -> if length (rekMen board src dst) == 0 then [[dst]] else map (dst :) (rekMen board src dst))
                        
rekMen :: Board -> Pos -> Pos -> [[Pos]]
rekMen board src dst = captMoveMen (testMove board src dst) dst

posibleCapt :: Board -> Pos -> [[Pos]]
posibleCapt board src@(x1,y1) = map (flip (:) []) $ fun . filter (isEmpty board) . filter (isLegal) $ map (plus src) $ map (flip prod 2) captM
                    where fun = filter (\(x2,y2) -> oposite (takeC board src) (takeC board ((x1+x2) `div` 2, (y1+y2) `div` 2)))

-------------------------Ruch Krolowki------------------------                    
moveKing :: Board -> Pos -> [[Pos]]
moveKing board src = if isCapture board src then bestMove $ captMoveKing board src else noCaptMoveKing board src
                      
noCaptMoveKing :: Board -> Pos -> [[Pos]]
noCaptMoveKing board src@(x1,y1) = map (flip (:) []) $ filter (\x -> and [(isEmpty board x), (countBetween board White src x == 0), (countBetween board Black src x == 0)]) fields 
                        where                          
                        fields = [(c, d) | c <- [1..8], d <- [1..8], even $ c+d, abs(x1-c) == abs(y1-d)]
                      
captMoveKing :: Board -> Pos -> [[Pos]]
captMoveKing board src = concatMap magicFun (posibleCaptKing board src)
                        where 
                        magicFun = (\[dst] -> if length (rekKing board src dst) == 0 then [[dst]] else map (dst :) (rekKing board src dst))
                        
rekKing :: Board -> Pos -> Pos -> [[Pos]]
rekKing board src dst = captMoveKing (testMove board src dst) dst

 
posibleCaptKing :: Board -> Pos -> [[Pos]]
posibleCaptKing board src@(x1,y1) = map (flip (:) []) $ fun . filter (isEmpty board) . filter (isLegal) $ fields
                                    where 
                                    fun = filter (\x -> and [(countBetween board (takeC board src) src x == 0), (countBetween board (reverseField $ takeC board src) src x == 1)]) 
                                    fields = [(c, d) | c <- [1..8], d <- [1..8], even $ c+d, abs(x1-c) == abs(y1-d)]
 
----------------------------------------------
countBetween :: Board -> Square -> Pos -> Pos -> Int
countBetween board field src dst = length $ filter (friend field) (map (takeC board) (fieldsBetween board src dst))

fieldsBetween :: Board -> Pos -> Pos -> [Pos]
fieldsBetween board (x1, y1) (x2, y2) = [(c, d) | c <- [1..8], d <- [1..8], even $ c+d, c > min x1 x2, c < max x1 x2, d > min y1 y2, d < max y1 y2, abs(x1-c) == abs(y1-d)]

-------------------------testMove == makeMove
testMove :: Board -> Pos -> Pos -> Board
testMove board src@(x,y) dst@(ox, oy) = moveC board2 src dst
                where board2 = foldr (\x acc -> deleteC x acc) board (fieldsBetween board src dst) 
                
----generuje nowa plansze po wykonaniu ruchu (uzywa tego, co zwroci allMoves)
makeMove :: Board -> [Pos] -> Board
makeMove board steps | length steps < 2 = board
                     | otherwise = makeMove (newBoard) (drop 1 steps)
                        where newBoard = testMove board (steps !! 0) (steps !! 1)
-------------------------------------------------
allMoves :: Board -> Square -> [[Pos]]
allMoves board color = bestMove $ concatMap (\x -> map (x:) (move board x)) checkers
                    where 
                    checkers = checkersOnBoard board color bool
                    bool = if (length $ checkersOnBoard board color True ) > 0 then True else False
                    
----pionki danego koloru na planszy, z kuciem lub bez
checkersOnBoard :: Board -> Square -> Bool -> [Pos]
checkersOnBoard board color capt | capt == False = [(c,d) | c<-[1..8], d<-[1..8],friend (takeC board (c,d)) color]
                                 | otherwise = [(c,d) | c<-[1..8], d<-[1..8],friend (takeC board (c,d)) color, isCapture board (c,d)]