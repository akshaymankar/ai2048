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
  geneticPlay g 1000
  --botPlay g 3
  --manualPlay g

geneticPlay :: GameBoard -> Int -> IO ()
geneticPlay g n = do
  w <- generateAIWorld 50 16 [20, 4]
  foldl (\newW _ -> join (nextWorld g <$> newW)) (return w) [1..n]
  print "Done"


botPlay :: GameBoard -> Int -> IO ()
botPlay g generations = do
  w <- generateAIWorld 50 16 [20, 4]
  bs <- sortedScores w g
  print bs

manualPlay :: GameBoard -> IO ()
manualPlay g = do
  hSetBuffering stdin NoBuffering
  humanGameLoop g

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
