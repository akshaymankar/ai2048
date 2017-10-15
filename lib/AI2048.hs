module AI2048 where

import Game2048
import Brain
import GHC.Exts
import System.IO.Unsafe

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
