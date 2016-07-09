module Main where

import Minmax
import Moves
import Board

deep :: Int   --have to be even
deep = 4

main :: IO()
main = do
        putStrLn "Choose board: \n 1. 3 rows\n 2. 2 rows \n (if you choose any other you will play with two rows of checkers)"
        x <-getLine
        if x=="1" then game initialBoard else game initB2


game :: Board -> IO ()
game initBoard = do
        putStrLn "Player : White ::\n 1.Human\n 2.AI\n(if you choose any other AI will play)"
        w<-getLine
        let w2 = (if (w =="1") then 1 else 2)
        putStrLn "Player : Black ::\n 1.Human\n 2.AI\n(if you choose any other AI will play)"
        b<-getLine
        let b2 = (if b=="1" then 1 else 2)
        play w2 b2 initBoard

play :: Int -> Int -> Board -> IO ()      
play w b board = do 
    putStr "Turn : White"
    board2 <- if w == 1 then playHuman board White else playAI board White
    if checkEnd board2 
      then 
        do endGame board2 
           if boardStateSum board2 White > 0 then putStrLn "** Won: White! **" else putStrLn "** Won: Black! **" 
      else
        do putStr "Turn : Black"
           board3 <- if b ==1 then playHuman board2 Black else playAI board2 Black
           if checkEnd board3 
             then do
               endGame board3 
               if boardStateSum board3 Black >0 then putStrLn "** Won: Black! **" else putStrLn "** Won: White! **"
             else
               play w b board3
                        
playAI :: Board -> Square -> IO Board 
playAI board color= do 
            putStrLn $ showBoard board
            let newBoard = funMinMax (takeN deep $ gameTree board color) deep
            return newBoard

playHuman :: Board -> Square -> IO Board            
playHuman board color = do 
            putStrLn $ showBoard board ++ "\n Choose move:"
            let moves = allMoves board color
            mapM_ putStr $ [show a ++ ") " ++show x ++ "\n"| (a,x) <- (zip [1..(length moves)] moves)]
            a <- getLine
            if not $ (read a) `elem` [1..(length moves)] 
              then putStrLn "*** Wrong move!!! ***\n*** Choose again: ***" >> playHuman board color
              else do
                let newBoard = check4Kings $ makeMove board (moves !! ((read a) - 1))
                return newBoard

-----sprawdzanie konca gry
checkEnd :: Board -> Bool          
checkEnd board | allMoves board Black == [] = True
               | allMoves board White == [] = True
               | otherwise = False

endGame :: Board -> IO ()               
endGame board = (putStrLn $ showBoard board) >> putStrLn "*** GAME OVER ***"