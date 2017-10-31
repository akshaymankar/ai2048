{-# LANGUAGE ImplicitParams #-}
module BrainSpec where

import Brain
import Test.Hspec
import Test.HUnit.Approx
import Control.Monad

{-# ANN module ("HLint: ignore Redundant do"::String) #-}
{-# ANN module ("HLint: ignore Use head"::String) #-}
{-# ANN module ("HLint: ignore Reduce duplication"::String) #-}

shouldApproxEqualEach :: (?epsilon :: Float) => [Float] -> [Float] -> IO ()
shouldApproxEqualEach xs ys = mconcat <$> zipWithM (@~?) ys xs

spec :: Spec
spec = let ?epsilon = 0.000001 in do
  describe "value" $ do
    it "should calculate value for a nueron" $ do
      let n = Neuron [0.5, 0.75] 3
      value [3, 4] n `shouldBe` 0.99944717

  describe "Layer#stimulate" $ do
    it "should calculate values for the layer" $ do
      let l = Layer [ Neuron [0.1, 0.4] 3,  Neuron [0.5, 0.75] 7, Neuron [0, 1] (-2)]
      stimulate [3, 4] l `shouldApproxEqualEach` [0.9926085, 0.99998987, 0.880797]

  describe "Brain#stimulate" $ do
    it "should calculate values for each layer while feeding last layer" $ do
      let l1 = Layer [ Neuron [0.1, 0.4] 3,  Neuron [0.5, 0.75] 7, Neuron [0, 1] (-2)]
      let l2 = Layer [ Neuron [0.3, 0.7, 1] 5 ]
      let b = Brain [l1, l2]
      stimulate [3, 4] b `shouldApproxEqualEach` [0.99897146]

  describe "GenerateRandomNeuron" $ do
    it "should generate a random neuron with given number of weights" $ do
      (Neuron weights _) <- generateRandomNeuron 4
      length weights `shouldBe` 4

  describe "GenerateRandomLayer" $ do
    it "should generate a random Layer with given number of neurons and inputs" $ do
      (Layer neurons) <- generateRandomLayer 2 4
      length neurons `shouldBe` 4
      map (length . weights) neurons `shouldBe` [2, 2, 2, 2]

  describe "GenerateRandomBrain" $ do
    it "should generate a random Brain with given number of layers" $ do
      (Brain layers) <- generateRandomBrain 1 [4, 2, 3, 1]
      length layers `shouldBe` 4
    it "should generate each layer with inputs from previos layers" $ do
      (Brain layers) <- generateRandomBrain 1 [4, 2, 3, 1]
      length (neurons $ head layers) `shouldBe` 4
      length (neurons $ layers !! 1) `shouldBe` 2
      length (neurons $ layers !! 2) `shouldBe` 3
    it "should generate each layer with inputs from previos layers" $ do
      (Brain layers) <- generateRandomBrain 1 [4, 2, 3, 1]
      numberOfWeightsOfNeurons (layers !! 0) `shouldBe` [1, 1, 1, 1]
      numberOfWeightsOfNeurons (layers !! 1) `shouldBe` [4, 4]
      numberOfWeightsOfNeurons (layers !! 2) `shouldBe` [2, 2, 2]
      numberOfWeightsOfNeurons (layers !! 3) `shouldBe` [3]
  describe "GenerateEnhancedBrain" $ do
    it "should generate have same number of layers as input brains" $ do
      inputBrain <- generateRandomBrain 1 [4, 2, 3, 1]
      (Brain layers) <- generateEnhancedBrain inputBrain inputBrain
      length layers `shouldBe` 4
    it "should generate layers with same number of neurons as input brains" $ do
      inputBrain <- generateRandomBrain 1 [4, 2, 3, 1]
      (Brain layers) <- generateEnhancedBrain inputBrain inputBrain
      length (neurons $ head layers) `shouldBe` 4
      length (neurons $ layers !! 1) `shouldBe` 2
      length (neurons $ layers !! 2) `shouldBe` 3
    it "should generate neurons with weights for previous layers" $ do
      inputBrain <- generateRandomBrain 1 [4, 2, 3, 1]
      (Brain layers) <- generateEnhancedBrain inputBrain inputBrain
      numberOfWeightsOfNeurons (layers !! 0) `shouldBe` [1, 1, 1, 1]
      numberOfWeightsOfNeurons (layers !! 1) `shouldBe` [4, 4]
      numberOfWeightsOfNeurons (layers !! 2) `shouldBe` [2, 2, 2]
      numberOfWeightsOfNeurons (layers !! 3) `shouldBe` [3]

  describe "crossover" $ do
    it "should generate n children for each pair" $ do
      b1 <- generateRandomBrain 1 [4, 2, 3, 1]
      b2 <- generateRandomBrain 1 [4, 2, 3, 1]
      newBrains <- crossover [b1, b2] [b1, b2] 2
      length newBrains `shouldBe` 4

numberOfWeightsOfNeurons layer = map (length . weights) (neurons layer)
