module AI2048 where

import Game2048
import Brain
import GHC.Exts
import System.IO.Unsafe

playGame :: GameBoard -> Brain -> Int -> IO GameBoard
playGame g b 0 = pure g
playGame g b turnsRemaining = do
  newG <- step g $ nextStep g b
  playGame newG b (turnsRemaining - 1)

nextStep :: GameBoard -> Brain -> Move
nextStep g b = readAction $ stimulate (toStimulus g) b

toStimulus :: GameBoard -> [Float]
toStimulus g = map fromIntegral $ concat $ toMatrix g

readAction :: [Float] -> Move
readAction fs = sneakyPrint $ (snd . last) $ sortWith fst $ zip fs [MoveRight, MoveDown, MoveLeft, MoveUp]

sneakyPrint :: Show a => a -> a
sneakyPrint a = unsafePerformIO $ do
  print a
  return a
