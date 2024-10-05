import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck
import           Control.Exception              ( evaluate )
import PegSolitaire

main :: IO ()
main = hspec $ do
  describe "isWinning" $ do
    it "should return False when the current state isn't a winning state" $ do
          let pegs = [Peg, Empty, Empty, Peg]
          isWinning pegs `shouldBe` False

    it "should return True when the current state is a winning state" $ do
          let pegs = [Peg, Empty, Empty, Empty]
          isWinning pegs `shouldBe` True

    it "should handle the empty list case" $ do
          let pegs = []
          isWinning pegs `shouldBe` False

  describe "generateStates" $ do
    it "should have tests" $ do
          (1 :: Integer) `shouldBe` (1 :: Integer)

  describe "generateLinearStates" $ do
    it "should have tests" $ do
          (1 :: Integer) `shouldBe` (1 :: Integer)

  describe "fromZipper" $ do
    it "should have tests" $ do
          (1 :: Integer) `shouldBe` (1 :: Integer)

  describe "toZipper" $ do
    it "should have tests" $ do
          (1 :: Integer) `shouldBe` (1 :: Integer)

  describe "tryRight" $ do
    it "should have tests" $ do
          (1 :: Integer) `shouldBe` (1 :: Integer)

  describe "tryLeft" $ do
    it "should have tests" $ do
          (1 :: Integer) `shouldBe` (1 :: Integer)

  describe "makeMoves" $ do
    it "should have tests" $ do
          (1 :: Integer) `shouldBe` (1 :: Integer)

  describe "foldT" $ do
    it "should have tests" $ do
          (1 :: Integer) `shouldBe` (1 :: Integer)

  describe "unfoldT" $ do
    it "should have tests" $ do
          (1 :: Integer) `shouldBe` (1 :: Integer)

  describe "makeGameTree" $ do
    it "should have tests" $ do
          (1 :: Integer) `shouldBe` (1 :: Integer)

  describe "hasSolution" $ do
    it "should have tests" $ do
          (1 :: Integer) `shouldBe` (1 :: Integer)

  describe "allSolutions" $ do
    it "should have tests" $ do
          (1 :: Integer) `shouldBe` (1 :: Integer)

  describe "getSolution" $ do
    it "should have tests" $ do
          (1 :: Integer) `shouldBe` (1 :: Integer)
