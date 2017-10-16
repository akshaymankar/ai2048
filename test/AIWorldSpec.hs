module AIWorldSpec where

import Test.Hspec
import AIWorld
import Brain
import Game2048
import AI2048

{-# ANN module ("HLint: ignore Redundant do"::String) #-}

spec :: Spec
spec = do
  describe "AIWorld" $ do
    describe "generateRandomWorld" $ do
      it "should generate a world with given population" $ do
        (AIWorld bs) <- generateAIWorld 50 1 [3,4]
        length bs `shouldBe` 50

    describe "generateNextWorld" $ do
      let g = fromMatrix [[0,0,0,0],[2,2,2,2],[0,0,0,0],[0,0,0,0]]
      it "should generate another world with same population" $ do
        w1 <- generateAIWorld 50 1 [3,4]
        (AIWorld bs) <- nextWorld g w1
        length bs `shouldBe` 50

      it "should generate another world with similar brains" $ do
        w1 <- generateAIWorld 50 1 [3,4]
        (AIWorld bs) <- nextWorld g w1
        (inputs . head) bs `shouldBe` 1
