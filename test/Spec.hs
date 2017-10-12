import Test.Hspec
import Game2048
{-# ANN module ("HLint: ignore Redundant do"::String) #-}

main :: IO()
main = hspec $ do
  describe "Game2048" $ do
    describe "addRow" $ do
      it "should add [x,x,y,y] to [x+x, y+y]" $ do
        addRow [2,2,4,4] `shouldBe` [4,8]

      it "should add [x,x,y,z] to [x+x, y, z]" $ do
        addRow [2,2,4,8] `shouldBe` [4,4,8]

      it "should add [a,x,x,z] to [a, x+x, z]" $ do
        addRow [2,4,4,8] `shouldBe` [2,8,8]

      it "should add [a,b,x,x] to [a, b, x+x, 0]" $ do
        addRow [2,4,8,8] `shouldBe` [2,4,16]

      it "should add [a,b,c,d] to [a, b, c, d]" $ do
        addRow [2,4,8,16] `shouldBe` [2,4,8,16]

    describe "moveLeft" $ do
      it "should not do anything to empty row" $ do
        moveLeft [0, 0, 0, 0] `shouldBe` [0, 0, 0, 0]

      it "should remove all the zeros from the left" $ do
        moveLeft [0, 0, 2, 0] `shouldBe` [2, 0, 0, 0]
      it "should retain all the non zeros" $ do
        moveLeft [0, 4, 2, 8] `shouldBe` [4, 2, 8, 0]
      it "should remove zeros between non zeros" $ do
        moveLeft [0, 4, 0, 8] `shouldBe` [4, 8, 0, 0]

      it "should add same adjacent numbers" $ do
        moveLeft [2, 2, 0, 0] `shouldBe` [4, 0 , 0 , 0]

      it "should add left most numbers first" $ do
        moveLeft [0, 2, 2, 2] `shouldBe` [4, 2 , 0 , 0]

    describe "moveRight" $ do
      it "should not do anything to empty row" $ do
        moveRight [0, 0, 0, 0] `shouldBe` [0, 0, 0, 0]

      it "should remove all the zeros from the right" $ do
        moveRight [0, 0, 2, 0] `shouldBe` [0, 0, 0, 2]
      it "should retain all the non zeros" $ do
        moveRight [4, 2, 8,0] `shouldBe` [0, 4, 2, 8]
      it "should remove zeros between non zeros" $ do
        moveRight [0, 4, 0, 8] `shouldBe` [0, 0, 4, 8]

      it "should add same adjacent numbers" $ do
        moveRight [0, 2, 2, 0] `shouldBe` [0, 0 , 0 , 4]

      it "should add right most numbers first" $ do
        moveRight [0, 2, 2, 2] `shouldBe` [0, 0 , 2 , 4]


    describe "shiftBoard" $ do
      describe "moveDown" $ do
        it "should not do anything to empty board" $ do
          shiftBoard emptyGameBoard MoveDown `shouldBe` emptyGameBoard

        it "should remove the zeros of bottom row" $ do
          let gameboard = fromMatrix [[0,0,0,0],[0,0,0,0],[2,2,2,2],[0,0,0,0]]
          let expectedGameboard = fromMatrix [[0,0,0,0],[0,0,0,0],[0,0,0,0],[2,2,2,2]]
          shiftBoard gameboard MoveDown `shouldBe` expectedGameboard

        it "should add the adjacent equal numbers" $ do
          let gameboard = fromMatrix [[0,0,4,16],[2,0,4,0],[2,2,2,0],[0,0,0,0]]
          let expectedGameboard = fromMatrix [[0,0,0,0],[0,0,0,0],[0,0,8,0],[4,2,2,16]]
          shiftBoard gameboard MoveDown `shouldBe` expectedGameboard

        it "should add the bottom most numbers first" $ do
          let gameboard = fromMatrix [[0,0,0,0],[0,0,0,2],[0,0,0,2],[0,0,0,2]]
          let expectedGameboard = fromMatrix [[0,0,0,0],[0,0,0,0],[0,0,0,2],[0,0,0,4]]
          shiftBoard gameboard MoveDown `shouldBe` expectedGameboard

      describe "moveUp" $ do
        it "should not do anything to empty board" $ do
          shiftBoard emptyGameBoard MoveUp `shouldBe` emptyGameBoard

        it "should remove the zeros of top row" $ do
          let gameboard = fromMatrix [[0,0,0,0],[2,2,2,2],[0,0,0,0],[0,0,0,0]]
          let expectedGameboard = fromMatrix [[2,2,2,2],[0,0,0,0],[0,0,0,0],[0,0,0,0]]
          shiftBoard gameboard MoveUp `shouldBe` expectedGameboard

        it "should add the adjacent equal numbers" $ do
          let gameboard = fromMatrix [[0,0,4,16],[2,0,4,0],[2,2,2,0],[0,0,0,0]]
          let expectedGameboard = fromMatrix [[4,2,8,16],[0,0,2,0],[0,0,0,0],[0,0,0,0]]
          shiftBoard gameboard MoveUp `shouldBe` expectedGameboard

        it "should add the top most numbers first" $ do
          let gameboard = fromMatrix [[0,0,0,0],[0,0,0,2],[0,0,0,2],[0,0,0,2]]
          let expectedGameboard = fromMatrix [[0,0,0,4],[0,0,0,2],[0,0,0,0],[0,0,0,0]]
          shiftBoard gameboard MoveUp `shouldBe` expectedGameboard

    describe "placeAt" $ do
      it "should place a number at the first cell" $ do
        placeAt emptyGameBoard 0 2 `shouldBe` fromMatrix [[2,0,0,0],[0,0,0,0],[0,0,0,0],[0,0,0,0]]

      it "should place a number at a given place" $ do
        placeAt emptyGameBoard 4 2 `shouldBe` fromMatrix [[0,0,0,0],[2,0,0,0],[0,0,0,0],[0,0,0,0]]

      it "should place a number at the last cell" $ do
        placeAt emptyGameBoard 15 2 `shouldBe` fromMatrix [[0,0,0,0],[0,0,0,0],[0,0,0,0],[0,0,0,2]]
