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
    Zipper(..),
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


-- | Zipper data type for storing a list in a focused structure.
-- The `Zipper` stores:
-- * A list of elements to the left of the current focus.
-- * The current focus item.
-- * A list of elements to the right of the current focus.
data Zipper a = Zipper [a] a [a]
    deriving (Show, Eq)



-- | Convert a Zipper back to a list.
-- This function takes a Zipper as an argument and returns its corresponding list by
-- concatenating its history (reversed), the focus, and the remainder.
--
-- === __Parameters__
-- * `Zipper a` - The zipper to convert to a list.
--
-- === __Returns__
-- * A list of elements that represents the zipper.
--
-- === __Examples__
-- >>> fromZipper (Zipper [2,1] 3 [4,5])
-- [1,2,3,4,5]
--
fromZipper :: Zipper a -> [a]
fromZipper (Zipper left focus right) = reverse left ++ [focus] ++ right


-- | Conver a list into a zipper with the first element as the focus.
-- This function takes a list as an argument and converts it into a `Zippper`. If the
-- list is empty, it returns nothing.
--
-- === __Parameters__
-- * `[a]` - The list to convert to a zipper.
--
-- === __Returns__
-- * A `Zipper` with the first element as the focus, or `Nothing` if the list is empty.
--
-- === __Examples__
-- >>> toZipper [1,2,3,4,5]
-- Just [](1)[2,3,4,5]
-- >>> toZipper []
-- Nothing
--
toZipper :: [a] -> Maybe (Zipper a)
toZipper [] = Nothing
toZipper (x:xs) = Just (Zipper [] x xs)


-- Move the focus of the zipper one position to the right.
-- If there is a value to the right of the focus, move the focus to that position,
-- and append the value to the history list. Otherwise, return `Nothing` (if the zipper
-- cannot move right).
--
-- === __Parameters__
-- * `Zipper a` - The zipper to move right.
--
-- === __Returns__
-- * A new zipper with the focus moved to the right, or `Nothing` if the zipper cannot move right.
--
-- === __Examples__
-- >>> tryRight (Zipper [2,1] 3 [4,5])
-- Just [1,2,3](4)[5]
--
-- >>> tryRight (Zipper [] 5 [])
-- Nothing

tryRight :: Zipper a -> Maybe (Zipper a)
tryRight (Zipper left focus (r:rs)) = Just (Zipper (focus:left) r rs)
tryRight _ = Nothing


-- | Move the focus of the zipper one position to the left.
-- If there is a value to the left of the focus, move the focus to that position,
-- and append the value to the remainder list. Otherwise, return `Nothing` (if the zipper
-- cannot move left).
--
-- === __Parameters__
-- * `Zipper a` - The zipper to move left.
--
-- === __Returns__
-- * A new zipper with the focus moved to the left, or `Nothing` if the zipper cannot move left.
--
-- === __Examples__
-- >>> tryLeft (Zipper [2,1] 3 [4,5])
-- Just [1](2)[3,4,5]
--
-- >>> tryLeft (Zipper [] 5 [6,7])
-- Nothing
--
tryLeft :: Zipper a -> Maybe (Zipper a)
tryLeft (Zipper (l:ls) focus rs) = Just (Zipper ls l (focus:rs))
tryLeft (Zipper [] _ _)          = Nothing


-- | Generates all possible game states of length n with exactly one empty position.
-- This function generates all possible game states where n-1 positions are filled with pegs and one position is empty.
--
-- === __Parameters__
-- * `n` - The length of the game state.
--
-- === __Returns__
-- * A list of game states with exactly one empty position.
--
-- === __Examples__
--
-- >>> generateLinearStates 2
-- [ .  X , X  . ]
--
-- >>> generateLinearStates 3
-- [ .  X  X , X  .  X , X  X  . ]
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
-- The function uses a bitmask approach to represent each state, where `0` represents an `Empty` slot, and `1` represents a `Peg`.
--
-- === __Parameters__
-- * `n` - The maximum length of the game state.
--
-- === __Returns__
-- * A list of all possible game states up to length n.
--
-- === __Examples__
--
-- >>> generateStates 2
-- [ . , X , .  . , X  . , .  X , X  X ]
--
-- >>> generateStates 3
-- [ . , X , .  . , X  . , .  X , X  X , .  .  . , X  .  . , .  X  . , X  X  . , .  .  X , X  .  X , .  X  X , X  X  X ]
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


-- | Generates all possible moves from a given game state.
--
-- === __Examples__
-- >>> makeMoves (Zipper [Peg, Empty, Peg] Empty [Peg, Peg])
makeMoves :: Zipper Peg -> [Zipper Peg]
makeMoves = error "Implement, document, and test this function"


-- | Unfolds a tree from a seed value.
--
-- === __Parameters__
-- * `f` - Function that generates the next tree node from a seed value.
-- * `x` - The seed value to start the tree generation.
--
-- === __Returns__
-- * A tree generated from the seed value.
--
-- === __Examples__
-- >>> unfoldT (\x -> if x == 0 then Left 0 else Right (x, [x-1])) 5
-- Node 5 [Node 4 [Node 3 [Node 2 [Node 1 [Leaf 0]]]]]
--
-- >>> unfoldT (\x -> if x == 0 then Left 0 else Right (x, [x-1])) 0
-- Leaf 0
--
-- >>> unfoldT (\x -> if 2 * x + 1 > 7 then Left x else Right (x, [2 * x + 1, 2 * x + 2])) 1
-- Node 1 [Node 3 [Leaf 7,Leaf 8],Leaf 4]
--
-- === __Notes__
-- Our implementation of `unfoldT` is an anamorphism factory for the `Tree` data type defined
-- above. `unfoldr`, for example, is an anamorphism factory for lists, and is used to build
-- lists from a seed value. `unfoldT` has a similar recursive structure to `unfoldr`, but can
-- only be applied to trees. `unfoldTree` from `Data.Tree` is more similar to our `unfoldT`.
--
unfoldT :: (b -> Either a (a, [b])) -> b -> Tree a
unfoldT f x = case f x of
    Left a -> Leaf a
    Right (a, bs) -> Node a (map (unfoldT f) bs)



makeGameTree = error "Implement, document, and test this function"
hasSolution = error "Implement, document, and test this function"
allSolutions = error "Implement, document, and test this function"
getSolution = error "Implement, document, and test this function"
trySolution = error "Implement, document, and test this function"
