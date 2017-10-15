module AI2048 where

import Game2048
import Brain
import GHC.Exts
import System.IO.Unsafe
import AIWorld

instance AIProblem GameBoard where
  fitness g b = score <$> playGame g 100 b

playGame :: GameBoard -> Int -> Brain -> IO GameBoard
playGame g 0 _ = pure g
playGame g turnsRemaining b  = do
  newG <- step g $ nextStep g b
  playGame newG (turnsRemaining - 1) b

nextStep :: GameBoard -> Brain -> Move
nextStep g b = readAction $ stimulate (toStimulus g) b

toStimulus :: GameBoard -> [Float]
toStimulus g = map fromIntegral $ concat $ toMatrix g

readAction :: [Float] -> Move
readAction fs = (snd . last) $ sortWith fst $ zip fs [MoveRight, MoveDown, MoveLeft, MoveUp]

sneakyPrint :: Show a => a -> a
sneakyPrint a = unsafePerformIO $ do
  print a
  return a
