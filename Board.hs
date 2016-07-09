--WARCABY!!!!!

module Board where
import Data.List

------------Types, Structures----------------------------

data Square = Empty | White | Black | WhiteKing | BlackKing deriving (Eq )
type Board = [[Square]]
type Pos = (Int,Int)

------------Showing, Reading------------------------------

instance Show Square where
    show Empty = "."
    show White = "w"
    show Black = "b"
    show WhiteKing = "W"
    show BlackKing = "B"
    
drawBoard :: Board -> IO ()
drawBoard = putStrLn . showBoard
        
showBoard :: Board -> String
showBoard  = prettyBoard  . flip (++) " 12345678\n"  . unlines . zipWith (:) ['8','7'..'1'] . (map $ concatMap show) 

prettyBoard :: String -> String
prettyBoard  = (++) "\n " . intersperse ' ' 

readSquare :: Char -> Square
readSquare x | x == '.' = Empty
             | x == 'b' = Black
             | x == 'w' = White 
             | x == 'B' = BlackKing
             | x == 'W' = WhiteKing
            
readBoard :: String -> Board
readBoard = (map $ map readSquare) . lines 

-------------Board Manipulations------------------------------------

changeField :: Board -> Pos -> Square ->Board
changeField board (y,x) a = 
    let headB = take (8-x) board
        tailB = drop (9-x) board
        line = board !! (8-x)
        headL = take (y-1) line
        tailL = drop y line
        newLine = headL ++ a : tailL
    in headB ++ newLine : tailB
   
deleteC :: Pos -> Board -> Board
deleteC a board = changeField board a Empty

addC :: Pos -> Square -> Board -> Board
addC a b board = changeField board a b

moveC :: Board -> Pos -> Pos -> Board
moveC board src dst = (addC dst checker) . (deleteC src) $ board
                    where checker = takeC board src 
                    
takeC :: Board -> Pos -> Square
takeC board (y,x)  = (board !! (8-x)) !! (y-1)


--------------Boards--------------

initialBoard, initB2, emptyBoard, s :: Board
initialBoard = readBoard ".b.b.b.b\nb.b.b.b.\n.b.b.b.b\n........\n........\nw.w.w.w.\n.w.w.w.w\nw.w.w.w."
initB2 = readBoard ".b.b.b.b\nb.b.b.b.\n........\n........\n........\n........\n.w.w.w.w\nw.w.w.w."
s = readBoard ".b.b.b.b\nb.b.b.b.\n.b.b.b.B\n........\n...b....\nw.w.w.w.\n...w...w\nw.w.w.w."
emptyBoard = [[Empty | _<-[1..8]] | _ <- [1..8]]