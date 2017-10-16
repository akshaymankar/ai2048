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
  return $ reverse $ sortWith snd $ zip bs scores

population :: AIWorld -> Int
population (AIWorld bs) = length bs

-- Keeps top 20% brains, crosses over top 40% brains with each other to get 80% of next world
nextWorld :: AIProblem a => a -> AIWorld -> IO AIWorld
nextWorld p w = AIWorld <$> do
  brainsAndScores <- sortedBrainsAndScores w p
  print $ map snd brainsAndScores
  let n = length brainsAndScores
      sbs = map fst brainsAndScores
      fortyPercentish = ceiling (0.4 * toRational n)
      notCrossedOver = n - (2 * fortyPercentish)
  crossedOver <- crossover (take fortyPercentish sbs) (take fortyPercentish sbs) 2
  return $ take notCrossedOver sbs ++ crossedOver
