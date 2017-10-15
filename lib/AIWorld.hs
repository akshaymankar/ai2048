module AIWorld where

import Brain
import Control.Monad
import GHC.Exts

newtype AIWorld = AIWorld [Brain]
  deriving Show

class AIProblem a where
  fitness :: a -> Brain -> IO Int

generateAIWorld :: Int -> Int -> [Int] -> IO AIWorld
generateAIWorld population inputs layers = AIWorld <$> replicateM population (generateRandomBrain inputs layers)

sortedBrains :: AIProblem a => AIWorld -> a -> IO [Brain]
sortedBrains world problem = map fst <$> sortedBrainsAndScores world problem

sortedScores :: AIProblem a => AIWorld -> a -> IO [Int]
sortedScores world problem = map snd <$> sortedBrainsAndScores world problem

sortedBrainsAndScores :: AIProblem a => AIWorld -> a -> IO [(Brain, Int)]
sortedBrainsAndScores (AIWorld bs) work = do
  scores <- mapM (fitness work) bs
  return $ sortWith snd $ zip bs scores
