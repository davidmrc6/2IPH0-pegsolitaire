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

  describe "foldT" $ do

    it "should transform a tree of numbers by summing them up" $ do
      let tree = Node 3 [Leaf 1, Leaf 2, Node 4 [Leaf 5]]
      foldT id (\n ts -> n + sum ts) tree `shouldBe` 15

    it "should transform a tree of strings by calculating total length" $ do
      let tree = Node "root" [Leaf "a", Leaf "bb", Leaf "ccc"]
      foldT length (\_ ts -> sum ts) tree `shouldBe` 6

    it "should handle a single-leaf tree" $ do
      let tree = Leaf 10
      foldT id (\n _ -> n) tree `shouldBe` 10

    it "should handle a single-node tree with no children" $ do
      let tree = Node 5 []
      foldT id (\n ts -> n + sum ts) tree `shouldBe` 5

    it "should apply custom function on a complex tree structure" $ do
      let tree = Node 2 [Leaf 1, Node 3 [Leaf 4, Leaf 5], Leaf 6]
      foldT show (\n ts -> concat ([show n] ++ ts)) tree `shouldBe` "213456"

    it "should return the correct sum for an empty node list" $ do
      let tree = Node 10 []
      foldT id (\n _ -> n) tree `shouldBe` 10


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
