{-# LANGUAGE ImplicitParams #-}
module BrainSpec where

import Brain
import Test.Hspec
import Test.HUnit.Approx
import Control.Monad

{-# ANN module ("HLint: ignore Redundant do"::String) #-}
{-# ANN module ("HLint: ignore Use head"::String) #-}

shouldApproxEqualEach :: (?epsilon :: Float) => [Float] -> [Float] -> IO ()
shouldApproxEqualEach xs ys = mconcat <$> zipWithM (@~?) xs ys

spec :: Spec
spec = let ?epsilon = 0.000001 in do
  describe "value" $ do
    it "should calculate value for a nueron" $ do
      let n = Neuron [0.5, 0.75]
      value [3, 4] n `shouldBe` 4.5

  describe "Layer#stimulate" $ do
    it "should calculate values for the layer" $ do
      let l = Layer [ Neuron [0.1, 0.4],  Neuron [0.5, 0.75], Neuron [0, 1]]
      stimulate [3, 4] l `shouldApproxEqualEach` [1.9, 4.5, 4]

  describe "Brain#stimulate" $ do
    it "should calculate values for each layer while feeding last layer" $ do
      let l1 = Layer [ Neuron [0.1, 0.4],  Neuron [0.5, 0.75], Neuron [0, 1]]
      let l2 = Layer [ Neuron [0.3, 0.7, 1] ]
      let b = Brain [l1, l2]
      stimulate [3, 4] b `shouldApproxEqualEach` [7.72]

  describe "GenerateRandomNeuron" $ do
    it "should generate a random neuron with given number of coefficients" $ do
      (Neuron coefficients) <- generateRandomNeuron 4
      length coefficients `shouldBe` 4

  describe "GenerateRandomLayer" $ do
    it "should generate a random Layer with given number of neurons and inputs" $ do
      (Layer neurons) <- generateRandomLayer 2 4
      length neurons `shouldBe` 4
      map (length . coefficients) neurons `shouldBe` [2, 2, 2, 2]

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
      numberOfCoefficientsOfNeurons (layers !! 0) `shouldBe` [1, 1, 1, 1]
      numberOfCoefficientsOfNeurons (layers !! 1) `shouldBe` [4, 4]
      numberOfCoefficientsOfNeurons (layers !! 2) `shouldBe` [2, 2, 2]
      numberOfCoefficientsOfNeurons (layers !! 3) `shouldBe` [3]

numberOfCoefficientsOfNeurons layer = map (length . coefficients) (neurons layer)
