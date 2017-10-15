module Main where

import Game2048
import System.IO
import AI2048
import Brain

main :: IO ()
main = do
    g <- initialBoard
    print g
    b <- generateRandomBrain 16 [20, 4]
    newG <- step g $ nextStep g b
    print newG
    --hSetBuffering stdin NoBuffering
    --gameLoop g

gameLoop :: GameBoard -> IO ()
gameLoop g = do
    x <- getChar
    putStrLn ""
    let action | x == 'w' = MoveUp
               | x == 's' = MoveDown
               | x == 'a' = MoveLeft
               | x == 'd' = MoveRight

    newG <- step g action
    print newG
    gameLoop newG
