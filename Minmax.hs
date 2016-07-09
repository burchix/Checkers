module Minmax where

import Moves
import Board
import Data.Tree
import Data.List
import Data.Function
import Control.Monad


--------------------Okreslanie stanu planszy
------ilosc pionkow*2+ ilosc damek * 5 + ilosc pionkow przy brzegu *2 + ilosc pionkow prawie przy brzegu
boardStateSum :: Board -> Square -> Int
boardStateSum board color =(+) (sign1 (boardState Black board)) (sign2 (boardState White board))
                        where
                        sign1 = if color == Black then id else negate
                        sign2 = if color == White then id else negate
                        
boardState :: Square -> Board -> Int
boardState color board = (sum $ map (foldl (\acc x -> if x == color then (acc+2) else if x==colorKing then (acc+5) else acc) 0) board) + x + x2
                    where 
                     x =(*) 2 $ length $ filter (friend color) [takeC board (c,d) | c<-[1..8], d<-[1..8], or [c `elem`[1,8], d `elem` [1,8]], isTaken board (c,d)]
                     x2 = length $ filter (friend color) [takeC board (c,d) | c<-[2..7], d<-[2..7], or [c `elem`[2,7], d `elem` [2,7]], isTaken board (c,d)]
                     colorKing = if color == Black then BlackKing else WhiteKing

-------------------zabawa drzewem.....------
makeTree :: Board -> Square -> Tree Board
makeTree b color = Node b (map (flip makeTree (reverseField color)) boards)
                    where boards = map (check4Kings . makeMove b) (allMoves b color)
                    
makeStateTree :: Tree Board -> Square -> Tree (Board, Int)
makeStateTree t color = fmap (\x -> (x, boardStateSum x color)) t 

gameTree :: Board -> Square -> Tree (Board, Int)
gameTree b c = makeStateTree (makeTree b c) c

takeN :: Int -> (Tree a) -> (Tree a)
takeN 0 (Node a _) = Node a []
takeN n (Node a x) = Node a (map (takeN (n-1)) x)

------------------MIN MAX z takeN
minmax :: Tree (Board,Int) -> Int -> Tree (Board,Int)
minmax (Node (b,score) xs) 0 = Node (b, score) []
minmax (Node (b,score) []) lvl |odd lvl = Node (b,1000) []
                               |even lvl = Node (b,-1000) []
                               
minmax (Node (b,score) xs) lvl | odd lvl = Node (b, snd $ minimumBy (compare `on` snd) $ concatMap (\x -> (levels x) !! 0) xs2) xs2
                               | even lvl = Node (b, snd $ maximumBy (compare `on` snd) $ concatMap (\x -> (levels x) !! 0) xs2) xs2
                                    where xs2 = map (flip minmax (lvl-1)) xs
                                    
funMinMax :: Tree (Board,Int) -> Int -> Board
funMinMax t@(Node (b,score) xs) lvl = foldl (\acc (Node (a,i) _) -> if (optScore == i) then a else acc) [] xs2
                                      where (Node (board2, optScore) xs2) = (minmax t lvl) 

--------------
replace :: Eq a => a -> a -> [a] -> [a]
replace x y = map (\z -> if z == x then y else z)

check4Kings :: Board -> Board
check4Kings b =  whiteKings : middle ++ [blackKings]
               where blackKings = replace Black BlackKing (b !! 7)
                     whiteKings = replace White WhiteKing (b !! 0)
                     middle = drop 1 $ take 7 b