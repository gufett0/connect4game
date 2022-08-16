module Main where

import MyGame
import Data
import Control.Applicative

main :: IO ()
main = do
  putStrLn "\nWelcome to my implementation of Connect 4 Game!"
  let start = initialBoardState 6 9
  putStrLn ("\n" ++ liftA2 (++) showBoard showTrailer start)
  playConnect4 start
  
playConnect4 :: BoardState -> IO ()
playConnect4 myboard
 | earlyTie myboard = putStrLn "The game is a TIE!"
 | anyColWins myboard || anyRowWins myboard = putStrLn "Congrats! \nLast move WON the game!"
 | otherwise = do 
    move <- getMove $ whoseTurn myboard
    let f = changeBoardState myboard 
        mynew_board = f $ makeMove myboard move
    putStrLn ("\n" ++ liftA2 (++) showBoard showTrailer mynew_board) 
    playConnect4 mynew_board
 
getMove :: Player -> IO Int
getMove player = do
                     putStr (show player ++ ",")
                     putStrLn " pick a column please: "
                     read <$> getLine

