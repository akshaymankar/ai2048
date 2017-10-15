module AISpec where

import Test.Hspec
import Game2048
import AI2048

{-# ANN module ("HLint: ignore Redundant do"::String) #-}

spec :: Spec
spec = do
  describe "AI2048" $ do
    describe "readAction" $ do
      it "should return MoveRight if the first neuron is max" $ do
        readAction [1, 0, 0, 0] `shouldBe` MoveRight
      it "should return MoveDown if the second neuron is max" $ do
        readAction [0, 1, 0, 0] `shouldBe` MoveDown
      it "should return MoveLeft if the third neuron is max" $ do
        readAction [0, 0, 1, 0] `shouldBe` MoveLeft
      it "should return MoveUp if the third neuron is max" $ do
        readAction [0, 0, 0, 1] `shouldBe` MoveUp

