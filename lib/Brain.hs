module Brain where

import System.Random
import Control.Monad
import Control.Applicative (pure)
import System.IO.Unsafe

newtype Neuron = Neuron { coefficients :: [Float] }
  deriving Show
newtype Layer = Layer { neurons :: [Neuron] }
  deriving Show
newtype Brain = Brain [Layer]
  deriving Show

value :: [Float] -> Neuron -> Float
value ns (Neuron cs) = sum $ zipWith (*) ns cs

class Stimulatable a where
  stimulate :: [Float] -> a -> [Float]

instance Stimulatable Layer where
  stimulate stimulus (Layer ns) = map (value stimulus) ns

instance Stimulatable Brain where
  stimulate stimulus (Brain ls) = foldl stimulate stimulus ls

generateRandomNeuron :: Int -> IO Neuron
generateRandomNeuron n = Neuron <$> replicateM n (randomRIO (0,1))

generateRandomLayer :: Int -> Int -> IO Layer
generateRandomLayer inputNeurons outputNeurons = Layer <$> replicateM outputNeurons (generateRandomNeuron inputNeurons)

generateRandomBrain :: Int -> [Int] -> IO Brain
generateRandomBrain inputs layers = Brain <$> zipWithM generateRandomLayer (inputs:layers) layers
