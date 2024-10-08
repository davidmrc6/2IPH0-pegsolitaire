{-|
Module      : PegSolitaire
Description :
Copyright   : David Mereacre (1966561)
              Ryan Sindic (1957198)


-}
module PegSolitaire
  (
    Peg(..),
    Pegs,
    stringToPegs,
    ----
    isWinning,
    generateStates,
    generateLinearStates,
    -- Zipper(..),
    fromZipper,
    toZipper,
    tryRight,
    tryLeft,
    ----
    makeMoves,
    foldT,
    unfoldT,
    Tree(..),
    makeGameTree,
    hasSolution,
    allSolutions,
    getSolution,
    trySolution,
  )
where
import Data.List (unfoldr)
import Data.Maybe
import Data.Bits (Bits(xor), testBit)

data Peg = Empty | Peg deriving (Eq, Ord)

type Pegs = [Peg]

data Tree a = Leaf a | Node a [Tree a] deriving (Show, Eq, Ord)

instance Show Peg where
  show Empty = "."
  show Peg = "X"

  showList xs = \s -> foldr (\ x-> (' ':) . shows x . (' ':)) s xs

stringToPegs :: String -> Pegs
stringToPegs = map f
  where
    f '.' = Empty
    f 'X' = Peg
    f _ = error "Invalid peg string"

--------------------------------------------------------------------------------------

-- | Function that, given a Peg solitaire game, determines if the current state is the winning state.
--
-- === __Parameters__
-- * `pegs` - A list of pegs representing the current state of the game.
--
-- === __Returns__
-- * `True` if the current state is a winning state, `False` otherwise.
--
-- === __Examples__
-- >>> isWinning [Peg, Empty, Empty, Empty, Peg, Peg]
-- False
--
-- >>> isWinning [Empty, Empty, Peg, Peg]
-- False
--
-- >>> isWinning [Empty, Peg, Emtpy, Empty]
-- True
--
isWinning :: Pegs -> Bool
isWinning pegs = (length . filter (== Peg)) pegs == 1


-- | Catamorphism factory for type `Tree` to help transform a tree into some other value.
-- The function recursively processes the tree structure, transforming it into a value of type `b`.
--
-- === __Parameters__
-- * `fLeaf` - Function to apply to the leaf nodes of the tree.
-- * `fNode` - Function to apply to the nodes of the tree.
-- * `Tree a` - The tree to transform.
--
-- === __Returns__
-- * A value of type `b` that represents the transformed tree.
--
-- === __Examples__
-- >>> foldT id (\n ts -> n + sum ts) (Node 3 [Leaf 1, Leaf 2, Node 4 [Leaf 5]])
-- 15
-- >>> foldT length (\_ ts -> sum ts) (Node "root" [Leaf "a", Leaf "bb", Leaf "ccc"])
-- 6
--
-- === __Notes__
-- Our implementation of `foldT` is a catamorphism factory for the `Tree` data type defined
-- above. `foldr`, for example, is a catamorphism factory for lists, and it has a similar
-- recursive structure to `foldT`, but can only be applied to lists. `foldTree` from `Data.Tree`
-- is more similar to our `foldT`, with a few important distinctions. `foldTree` uses one
-- function for both nodes and strings, as opposed to `foldT`, which explicitly separates
-- leaf and node processing.
--
foldT :: (a -> b) -> (a -> [b] -> b) -> Tree a -> b
foldT fLeaf fNode = go
    where
        go (Leaf l) = fLeaf l
        go (Node n ts) = fNode n (map go ts)


-- `Zipper` data type
data Zipper a = Zipper [a] a [a]

instance Show a => Show (Zipper a) where
    show (Zipper left focus right) =
        show (reverse left) ++ "(" ++ show focus ++ ")" ++ show right

fromZipper :: Zipper a -> [a]
fromZipper (Zipper left focus right) = reverse left ++ [focus] ++ right


toZipper :: [a] -> Maybe (Zipper a)
toZipper [] = Nothing
toZipper (x:xs) = Just (Zipper [] x xs)


tryRight :: Zipper a -> Maybe (Zipper a)
tryRight (Zipper left focus (r:rs)) = Just (Zipper (focus:left) r rs)
tryRight _ = Nothing


tryLeft :: Zipper a -> Maybe (Zipper a)
tryLeft (Zipper (l:ls) focus right) = Just (Zipper ls l (focus:right))
tryLeft _ = Nothing

-- | Generates all possible game states of length n with exactly one empty position.
--
-- This function generates all possible game states where n-1 positions are filled with pegs and one position is empty.
--
-- === __Examples__
--
-- >>> generateLinearStates 2
-- [[Empty,Peg],[Peg,Empty]]
--
-- >>> generateLinearStates 3
-- [[Empty,Peg,Peg],[Peg,Empty,Peg],[Peg,Peg,Empty]]
--
generateLinearStates :: Int -> [Pegs]
generateLinearStates n = unfoldr f 0
  where
    f :: Int -> Maybe (Pegs, Int)
    f i
      | i >= n = Nothing
      | otherwise = Just (pegsList i, i+1)
    pegsList :: Int -> Pegs
    pegsList i = [if j == i then Empty else Peg | j <- [0..n-1]]

-- | Generates all possible game states up to length n.
--
-- This function generates all possible game states of lengths from 1 up to n, where each position can be either a peg or empty.
--
-- === __Examples__
--
-- >>> generateStates 2
-- [[Empty],[Peg],[Empty,Empty],[Peg,Empty],[Empty,Peg],[Peg,Peg]]
--
generateStates :: Int -> [Pegs]
generateStates n = concatMap generateStatesOfLength [1..n]
  where
    generateStatesOfLength :: Int -> [Pegs]
    generateStatesOfLength k = unfoldr f 0
      where
        maxNum = 2^k - 1
        f :: Int -> Maybe (Pegs, Int)
        f i
          | i > maxNum = Nothing
          | otherwise = Just (intToPegs i k, i+1)
    intToPegs :: Int -> Int -> Pegs
    intToPegs i k = [if testBit i j then Peg else Empty | j <- [0..k-1]]

makeMoves = error "Implement, document, and test this function"
unfoldT = error "Implement, document, and test this function"
makeGameTree = error "Implement, document, and test this function"
hasSolution = error "Implement, document, and test this function"
allSolutions = error "Implement, document, and test this function"
getSolution = error "Implement, document, and test this function"
trySolution = error "Implement, document, and test this function"
