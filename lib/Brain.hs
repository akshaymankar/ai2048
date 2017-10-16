module Brain where

import System.Random
import Control.Monad
import Control.Applicative (pure)
import System.IO.Unsafe
import System.Random.Shuffle

newtype Neuron = Neuron { coefficients :: [Float] }
  deriving Show
newtype Layer = Layer { neurons :: [Neuron] }
  deriving Show
newtype Brain = Brain { layers :: [Layer] }
  deriving Show

value :: [Float] -> Neuron -> Float
value ns (Neuron cs) = sum $ zipWith (*) ns cs

class Stimulatable a where
  stimulate :: [Float] -> a -> [Float]

class Mateable a where
  mate :: a -> a -> IO a

instance Stimulatable Layer where
  stimulate stimulus (Layer ns) = map (value stimulus) ns

instance Stimulatable Brain where
  stimulate stimulus (Brain ls) = foldl stimulate stimulus ls

mutate :: Float -> IO Float
mutate x = do
  chance <- randomRIO (0, 1) :: IO Float
  mutation <- randomRIO (0, 1) :: IO Float
  if chance < 0.1 then return $ mutation * x
                  else return x

pick1 :: Float -> Float -> IO Float
pick1 x y = do
  returnX <- randomIO
  if returnX then return x
             else return y

instance Mateable Neuron where
  mate (Neuron cs1) (Neuron cs2) = Neuron <$> zipWithM pick1 cs1 cs2

instance Mateable Layer where
  mate (Layer ns1) (Layer ns2) = Layer <$> zipWithM mate ns1 ns2

instance Mateable Brain where
  mate (Brain ls1) (Brain ls2) = Brain <$> zipWithM mate ls1 ls2

generateRandomNeuron :: Int -> IO Neuron
generateRandomNeuron n = Neuron <$> replicateM n (randomRIO (0,1))

generateRandomLayer :: Int -> Int -> IO Layer
generateRandomLayer inputNeurons outputNeurons = Layer <$> replicateM outputNeurons (generateRandomNeuron inputNeurons)

generateRandomBrain :: Int -> [Int] -> IO Brain
generateRandomBrain inputs layers = Brain <$> zipWithM generateRandomLayer (inputs:layers) layers

inputs :: Brain -> Int
inputs (Brain []) = -1
inputs (Brain (l:_)) = (length . coefficients . head  . neurons) l

layerLengths :: Brain -> [Int]
layerLengths (Brain ls) = map (length . neurons) ls

generateEnhancedBrain :: Brain -> Brain -> IO Brain
generateEnhancedBrain b1 b2 = generateRandomBrain (inputs b1) (layerLengths b1)

randomPairs :: [a] -> [a] -> IO [(a,a)]
randomPairs xs ys = do
  newXs <- shuffleM xs
  newYs <- shuffleM ys
  return $ zip newXs newYs

crossover :: [Brain] -> [Brain] -> Int -> IO [Brain]
crossover bs1 bs2 n =  do
  pairs <- randomPairs bs1 bs2
  x <- mapM (\_ -> mapM (uncurry mate) pairs) [1..n]
  return $ concat x
