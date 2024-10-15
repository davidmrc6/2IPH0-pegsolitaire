import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck
import           Control.Exception              ( evaluate )
import           Data.Maybe                     ( fromJust )
import PegSolitaire

main :: IO ()
main = hspec $ do
  -- | Tests for `isWinning`
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

    it "should return False for a full list of pegs but not a winning state" $ do
        let pegs = [Peg, Peg, Peg, Peg]
        isWinning pegs `shouldBe` False

    it "should return True for a single peg (winning state)" $ do
        let pegs = [Peg]
        isWinning pegs `shouldBe` True

    it "should return False when all pegs are in an empty state" $ do
        let pegs = [Empty, Empty, Empty]
        isWinning pegs `shouldBe` False

    it "should return False for alternating Peg and Empty" $ do
        let pegs = [Peg, Empty, Peg, Empty]
        isWinning pegs `shouldBe` False


  -- | Tests for `foldT`
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

    it "should fold a tree with multiple nodes and leaves correctly" $ do
        let tree = Node 10 [Leaf 3, Node 5 [Leaf 7, Leaf 2], Leaf 1]
        foldT id (\n ts -> n + sum ts) tree `shouldBe` 28

    it "should handle deeply nested tree structures" $ do
        let tree = Node 10 [Node 20 [Leaf 5, Node 7 [Leaf 3, Leaf 2]]]
        foldT id (\n ts -> n + sum ts) tree `shouldBe` 47


  -- | Tests for `toZipper`
  describe "toZipper" $ do
    it "should convert a list into a zipper" $ do
        let list = [1,2,3,4,5] :: [Int]
        toZipper list `shouldBe` Just ( Zipper [] 1 [2,3,4,5])
    it "should handle an empty list" $ do
        let list = [] :: [Int]
        toZipper list `shouldBe` Nothing

    it "should convert a single-element list to a zipper" $ do
        let list = [1] :: [Int]
        toZipper list `shouldBe` Just (Zipper [] 1 [])

    it "should convert a longer list to a zipper" $ do
        let list = [1,2,3,4,5,6,7,8,9] :: [Int]
        toZipper list `shouldBe` Just (Zipper [] 1 [2,3,4,5,6,7,8,9])


  -- | Tests for `fromZipper`
  describe "fromZipper" $ do
    it "should convert a zipper into a list" $ do
        let zipper = Zipper [3,2,1] 4 [5,6,7]
        fromZipper zipper `shouldBe` [1,2,3,4,5,6,7]

    it "should handle an empty zipper" $ do
        let zipper = Zipper [] 0 []
        fromZipper zipper `shouldBe` [0]

    it "should handle a zipper with a single element" $ do
        let zipper = Zipper [] 5 []
        fromZipper zipper `shouldBe` [5]

    it "should return a list with only the focus when history and remainder are empty" $ do
        let zipper = Zipper [] 1 []
        fromZipper zipper `shouldBe` [1]


  -- | Tests for `tryRight`
  describe "tryRight" $ do
    it "should move focus one step to the right" $ do
      let zipper = Zipper [2,1] 3 [4,5]
      tryRight zipper `shouldBe` Just (Zipper (3:2:1:[]) 4 [5])

    it "should return Nothing if there is no element to the right" $ do
      let zipper = Zipper [] 5 []
      tryRight zipper `shouldBe` Nothing

    it "should return Nothing if focus is at the end of the list" $ do
        let zipper = Zipper [2,1] 3 []
        tryRight zipper `shouldBe` Nothing

    it "should move multiple steps to the right" $ do
        let zipper = Zipper [] 1 [2,3,4]
        tryRight zipper `shouldBe` Just (Zipper [1] 2 [3,4])
        case tryRight (Zipper [1] 2 [3,4]) of
            Just z -> tryRight z `shouldBe` Just (Zipper [3,2,1] 4 [])
            _ -> expectationFailure "Expected Just value"


  -- | Tests for `tryLeft`
  describe "tryLeft" $ do
    it "should move focus one step to the left" $ do
      let zipper = Zipper [2,1] 3 [4,5]
      tryLeft zipper `shouldBe` Just (Zipper [1] 2 [3,4,5])

    it "should return Nothing if there is no element to the left" $ do
      let zipper = Zipper [] 5 [6,7]
      tryLeft zipper `shouldBe` Nothing

    it "should return Nothing if focus is at the start of the list" $ do
        let zipper = Zipper [] 1 [2,3]
        tryLeft zipper `shouldBe` Nothing

    it "should move multiple steps to the left" $ do
        let zipper = Zipper [3,2,1] 4 [5]
        tryLeft zipper `shouldBe` Just (Zipper [2,1] 3 [4,5])
        case tryLeft (Zipper [2,1] 3 [4,5]) of
            Just z -> tryLeft z `shouldBe` Just (Zipper [] 1 [2,3,4,5])
            _ -> expectationFailure "Expected Just value"


  -- Tests for `generateStates`
  describe "generateStates" $ do
    it "should generate all states for n=1" $ do
      generateStates 1 `shouldBe` [[Empty], [Peg]]

    it "should generate all states up to n=3" $ do
      generateStates 3 `shouldBe` [[Empty], [Peg],
                                   [Empty, Empty],
                                   [Peg, Empty],
                                   [Empty, Peg],
                                   [Peg, Peg],
                                   [Empty, Empty, Empty],
                                   [Peg, Empty, Empty],
                                   [Empty, Peg, Empty],
                                   [Peg, Peg, Empty],
                                   [Empty, Empty, Peg],
                                   [Peg, Empty, Peg],
                                   [Empty, Peg, Peg],
                                   [Peg, Peg, Peg]]

    it "should generate all states up to n=2" $ do
      generateStates 2 `shouldBe` [[Empty], [Peg], [Empty, Empty], [Peg, Empty], [Empty, Peg], [Peg, Peg]]

    it "should handle large n values (e.g., n=4)" $ do
      let n = 4
      generateStates n `shouldBe` [[Empty], [Peg],
                                   [Empty, Empty], [Peg, Empty], [Empty, Peg], [Peg, Peg],
                                   [Empty, Empty, Empty], [Peg, Empty, Empty], [Empty, Peg, Empty], [Peg, Peg, Empty],
                                   [Empty, Empty, Peg], [Peg, Empty, Peg], [Empty, Peg, Peg], [Peg, Peg, Peg],
                                   [Empty, Empty, Empty, Empty], [Peg, Empty, Empty, Empty], [Empty, Peg, Empty, Empty],
                                   [Peg, Peg, Empty, Empty], [Empty, Empty, Peg, Empty], [Peg, Empty, Peg, Empty],
                                   [Empty, Peg, Peg, Empty], [Peg, Peg, Peg, Empty], [Empty, Empty, Empty, Peg],
                                   [Peg, Empty, Empty, Peg], [Empty, Peg, Empty, Peg], [Peg, Peg, Empty, Peg],
                                   [Empty, Empty, Peg, Peg], [Peg, Empty, Peg, Peg], [Empty, Peg, Peg, Peg], [Peg, Peg, Peg, Peg]]

    it "should generate an empty list when n=0" $ do
      generateStates 0 `shouldBe` []


  -- Tests for `generateLinearStates`
  describe "generateLinearStates" $ do
    it "should generate states for n=2 with exactly one empty position" $ do
        generateLinearStates 2 `shouldBe` [[Empty, Peg], [Peg, Empty]]

    it "should generate states for n=3 with exactly one empty position" $ do
        generateLinearStates 3 `shouldBe` [[Empty, Peg, Peg], [Peg, Empty, Peg], [Peg, Peg, Empty]]

    it "should generate an empty list when n=0" $ do
        generateLinearStates 0 `shouldBe` []

    it "should generate a single state when n=1" $ do
        generateLinearStates 1 `shouldBe` [[Empty]]

    it "should handle large n values" $ do
        let n = 4
        generateLinearStates n `shouldBe` [[Empty, Peg, Peg, Peg],
                                            [Peg, Empty, Peg, Peg],
                                            [Peg, Peg, Empty, Peg],
                                            [Peg, Peg, Peg, Empty]]



  describe "makeMoves" $ do
    it "should handle an empty game state" $ do
          makeMoves (Zipper [] Empty []) `shouldBe` []

    it "should handle a game state with only one Peg element" $ do
            makeMoves (Zipper [] Peg []) `shouldBe` []

    it "should handle a (non-empty) game state with no possible moves" $ do
        makeMoves (Zipper [Peg, Empty] Empty [Empty, Peg]) `shouldBe` []

    it "should make the right move on a simple game state" $ do
          makeMoves (Zipper [Peg] Peg [Empty]) `shouldBe` [Zipper [] Empty [Empty, Peg]]

    it "should make the right move on a more complex game state" $ do
        map fromZipper (makeMoves (Zipper [Peg, Peg] Peg [Empty, Empty])) `shouldBe` [[Peg, Empty, Empty, Peg, Empty]]

    it "should make the right moves on a game with multiple reachable game stats" $ do
        map fromZipper (makeMoves (Zipper [Peg, Peg, Empty] Peg [Empty, Peg, Peg, Empty])) `shouldBe` [
                [Empty, Peg, Peg, Peg, Peg, Empty, Empty, Empty],
                [Peg, Empty, Empty, Peg, Empty, Peg, Peg, Empty],
                [Empty, Peg, Empty, Empty, Peg, Peg, Peg, Empty],
                [Empty, Peg, Peg, Peg, Empty, Empty, Empty, Peg]
            ]

  describe "unfoldT" $ do
        it "unfolds a tree from a seed value with a simple decrement function" $ do
            let f x = if x == 0 then Left 0 else Right (x, [x-1])
            unfoldT f 5 `shouldBe` Node 5 [Node 4 [Node 3 [Node 2 [Node 1 [Leaf 0]]]]]

        it "unfolds a tree from a seed value with a binary tree function" $ do
            let f x = if 2 * x + 1 > 7 then Left x else Right (x, [2 * x + 1, 2 * x + 2])
            unfoldT f 1 `shouldBe` Node 1 [Node 3 [Leaf 7, Leaf 8], Leaf 4]

        it "unfolds a tree from a seed value with a single node" $ do
            let f = Left
            unfoldT f 0 `shouldBe` Leaf 0

  describe "makeGameTree" $ do
    it "generates the correct game tree for a simple initial state" $ do
        let initialState = Zipper [Peg, Peg] Empty []
        let expectedTree = Node (Zipper [Peg, Peg] Empty [])
                            [Leaf (Zipper [] Empty [Empty, Peg])]
        let actualTree = makeGameTree initialState
        actualTree `shouldBe` expectedTree

    it "generates the correct game tree for a more complex initial state" $ do
        let initialState = Zipper [Peg, Peg] Empty [Empty, Peg, Peg]
        let expectedTree = Node (Zipper [Peg, Peg] Empty [Empty, Peg, Peg])
                                    [ Node (Zipper [Empty, Peg, Empty, Peg, Peg] Empty [])
                                        [ Node (Zipper [] Empty [Empty, Peg, Peg, Empty, Empty])
                                            [ Leaf (Zipper [Empty, Peg, Empty] Empty [Empty, Empty])
                                            , Leaf (Zipper [Empty, Empty] Empty [Empty, Peg, Empty])
                                            ]
                                        ]
                                    , Node (Zipper [] Empty [Empty, Peg, Empty, Peg, Peg])
                                        [ Node (Zipper [Empty, Peg, Peg, Empty, Empty] Empty [])
                                            [ Leaf (Zipper [Empty, Peg, Empty] Empty [Empty, Empty])
                                            , Leaf (Zipper [Empty, Empty] Empty [Empty, Peg, Empty])
                                            ]
                                        ]
                                    ]
        makeGameTree initialState `shouldBe` expectedTree


    it "generates an empty tree for an already solved state" $ do
        let initialState = Zipper [Empty, Empty] Peg [Empty, Empty]
        let expectedTree = Leaf (Zipper [Empty, Empty] Peg [Empty, Empty])
        makeGameTree initialState `shouldBe` expectedTree

  describe "hasSolution" $ do
    it "returns True for a solvable game state [Peg, Peg, Empty, Peg]" $ do
        let initialState = fromJust $ toZipper [Peg, Peg, Empty, Peg]
        hasSolution initialState `shouldBe` True

    it "returns True for a game state which is already winning" $ do
        let initialState = fromJust $ toZipper [Empty, Empty, Peg, Empty]
        hasSolution initialState `shouldBe` True

    it "returns False for an unsolvable game state [Peg, Empty, Peg, Empty]" $ do
        let initialState = fromJust $ toZipper [Peg, Empty, Peg, Empty]
        hasSolution initialState `shouldBe` False

    it "should handle the empty list case" $ do
            let pegs = []
            isWinning pegs `shouldBe` False

    it "should return False for a full list of pegs but not a winning state" $ do
            let pegs = [Peg, Peg, Peg, Peg]
            isWinning pegs `shouldBe` False

    it "should return True for a single peg (winning state)" $ do
            let pegs = [Peg]
            isWinning pegs `shouldBe` True

    it "should return False when all pegs are in an empty state" $ do
            let pegs = [Empty, Empty, Empty]
            isWinning pegs `shouldBe` False

  describe "allSolutions" $ do
    it "should have tests" $ do
          (1 :: Integer) `shouldBe` (1 :: Integer)

  describe "getSolution" $ do
    it "should have tests" $ do
          (1 :: Integer) `shouldBe` (1 :: Integer)
