module Main where

import Game2048
import System.IO
import AI2048
import Brain
import AIWorld
import Control.Monad

main :: IO ()
main = do
  g <- initialBoard
  print g
  w <- generateAIWorld 50 16 [20, 4]
  bs <- sortedScores w g
  print bs
  --hSetBuffering stdin NoBuffering
  --humanGameLoop g

humanGameLoop :: GameBoard -> IO ()
humanGameLoop g = do
  x <- getChar
  putStrLn ""
  let action | x == 'w' = MoveUp
             | x == 's' = MoveDown
             | x == 'a' = MoveLeft
             | x == 'd' = MoveRight

  newG <- step g action
  print newG
  humanGameLoop newG
