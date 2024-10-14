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
import Data.Bits (testBit)

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


-- | Helper function that moves the focus to the start of the zipper (i. e., after applying the
-- function, the history will be empty).
--
-- === __Examples__
-- >>> moveFocusToLeft (Zipper [2, 1] 3 [4,5])
-- Zipper [] 1 [2,3,4,5]
--
moveFocusToLeft :: Zipper a -> Zipper a
moveFocusToLeft zipper = maybe zipper moveFocusToLeft (tryLeft zipper)


-- | Helper function that moves the focus to the end of the zipper (i. e., after applying the
-- function, the remainder will be empty).
--
-- >>> moveFocusToRight (Zipper [2, 1] 3 [4,5])
-- Zipper [4,3,2,1] 5 []
--
moveFocusToRight :: Zipper a -> Zipper a
moveFocusToRight zipper = maybe zipper moveFocusToRight (tryRight zipper)



-- | Generates all possible moves from a given game state.
--
-- This function generates all possible game states that can be reached from the current game state by moving a peg
-- over another peg. The function works by unfolding both the history and the remainder of the zipper,
-- moving the focus to the right and left, and generating a new zipper for each possible move.
--
-- === __Parameters__
-- * `zipper` - The current game state.
--
-- === __Returns__
-- * A list of all possible game states that can be reached from the current game state.
--
-- === __Examples__
-- >>> makeMoves (Zipper [Peg, Empty, Peg] Empty [Peg, Peg])
-- [Zipper  .  X  X  .  X  . ]
--
-- >>> makeMoves (Zipper [Peg, Empty, Peg] Peg [Peg, Peg])
-- [Zipper  .  X  X  .  X  X ]
--
-- >>> map fromZipper  (makeMoves (Zipper [Peg, Peg] Empty [Empty, Peg, Peg]))
-- [ X  X  .  X  .  . , .  .  X  .  X  X ]
--
-- >>> makeMoves (Zipper [Peg, Empty] Peg [])
-- [Zipper  .  X  . ]
--
makeMoves :: Zipper Peg -> [Zipper Peg]
makeMoves zipper = catMaybes (leftMoves ++ rightMoves)
    where

        leftMoves = unfoldr moveLeft (moveFocusToRight zipper)
        rightMoves = unfoldr moveRight (moveFocusToLeft zipper)

        moveLeft :: Zipper Peg -> Maybe (Maybe (Zipper Peg), Zipper Peg)
        moveLeft (Zipper (l1:l2:ls) focus rs)
         | l2 == Empty && l1 == Peg  && focus == Peg =
            Just (Just (Zipper (Empty:Peg:ls) Empty rs), Zipper (l2:ls) l1 (focus:rs))
         | otherwise = Just (Nothing, Zipper (l2:ls) l1 (focus:rs))
        moveLeft _ = Nothing

        moveRight :: Zipper Peg -> Maybe (Maybe (Zipper Peg), Zipper Peg)
        moveRight (Zipper ls focus (r1:r2:rs))
         | r2 == Empty && r1 == Peg && focus == Peg =
            Just (Just (Zipper ls Empty (Empty:Peg:rs)), Zipper  (focus:ls) r1 (r2:rs))
         | otherwise = Just (Nothing, Zipper (focus:ls) r1 (r2:rs))
        moveRight _ = Nothing


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


-- | Generates the game tree for a given initial state.
-- The tree is generated using the `unfoldT` function, where each node represents a game state
-- and its children represent the next possible game states that can be reached by making a valid move.
--
-- === __Parameters__
-- * `initialState` - The initial game state represented as a `Zipper Peg`.
--
-- === __Returns__
-- * A `Tree` representing all possible game states that can be reached from the initial state.
--
-- === __Examples__
-- >>> makeGameTree (Zipper [Peg, Peg] Empty [Empty, Peg, Peg])
-- Node (Zipper  X  X  .  .  X  X ) [Node (Zipper  .  X  .  X  X  . ) [Node (Zipper  .  .  X  X  .  . ) [Leaf (Zipper  .  X  .  .  .  . ),Leaf (Zipper  .  .  .  .  X  . )]],Node (Zipper  .  .  X  .  X  X ) [Node (Zipper  .  X  X  .  .  . ) [Leaf (Zipper  .  X  .  .  .  . ),Leaf (Zipper  .  .  .  .  X  . )]]]
--
makeGameTree :: Zipper Peg -> Tree (Zipper Peg)
makeGameTree = unfoldT f
    where
        f :: Zipper Peg -> Either (Zipper Peg) (Zipper Peg, [Zipper Peg])
        f zipper =
            let moves = makeMoves zipper
            in if null moves
                    then Left zipper
                    else Right (zipper, moves)

-- | Represents a move in the game, specifying the from, over, and to positions.
data Move = Move { from :: Int, over :: Int, to :: Int } deriving (Show, Eq)

-- | Determines if a starting game state has a solution using a hylomorphism.
-- This function uses `foldT` and `unfoldT` to process the game tree generated from the initial state.
--
-- === __Parameters__
-- * `initialState` - The initial game state represented as a `Zipper Peg`.
--
-- === __Returns__
-- * `True` if the game state has a solution, `False` otherwise.
--
-- === __Examples__
-- >>> hasSolution (fromJust $ toZipper [Peg, Peg, Empty, Peg])
-- True
--
-- >>> hasSolution (fromJust $ toZipper [Peg, Empty, Peg, Peg])
-- True
--
-- >>> hasSolution (fromJust $ toZipper [Empty, Empty, Peg, Empty])
-- False
--
hasSolution :: Zipper Peg -> Bool
hasSolution initialState = foldT fLeaf fNode (makeGameTree initialState)
  where
    fLeaf z = isWinning (fromZipper z)
    fNode _ bs = or bs

-- | Returns all possible winning end states as a hylomorphism.
-- This function collects all winning end states from the game tree generated from the initial state.
--
-- === __Parameters__
-- * `initialState` - The initial game state represented as a `Zipper Peg`.
--
-- === __Returns__
-- * A list of game states (as `Pegs`) that are winning end states.
--
-- === __Examples__
-- >>> allSolutions (fromJust $ toZipper [Peg, Peg, Empty, Peg])
-- [[Empty, Empty, Empty, Peg]]
--
-- >>> allSolutions (fromJust $ toZipper [Peg, Peg, Peg])
-- [[Empty, Empty, Peg], [Peg, Empty, Empty]]
--
allSolutions :: Zipper Peg -> [Pegs]
allSolutions initialState = foldT fLeaf fNode (makeGameTree initialState)
  where
    fLeaf z = [fromZipper z | isWinning (fromZipper z)]
    fNode _ bs = concat bs

-- | Represents a tree structure that includes moves leading to each node.
data MoveTree = MoveLeaf (Zipper Peg) | MoveNode (Zipper Peg) [(Move, MoveTree)] deriving (Show, Eq)

-- | Calculates the index of the focus in the zipper.
--
-- === __Parameters__
-- * `Zipper a` - The zipper to get the focus index from.
--
-- === __Returns__
-- * The index of the focus element.
--
getFocusIndex :: Zipper a -> Int
getFocusIndex (Zipper left _ _) = length left

-- | Generates all possible moves from a given game state, along with the moves made.
--
-- This function is similar to `makeMoves`, but also returns the move that led to each new game state.
--
-- === __Parameters__
-- * `zipper` - The current game state.
--
-- === __Returns__
-- * A list of tuples containing the move made and the resulting game state.
--
makeMovesWithMoves :: Zipper Peg -> [(Move, Zipper Peg)]
makeMovesWithMoves zipper = catMaybes (leftMoves ++ rightMoves)
  where
    leftMoves = unfoldr moveLeft (moveFocusToRight zipper)
    rightMoves = unfoldr moveRight (moveFocusToLeft zipper)

    moveLeft :: Zipper Peg -> Maybe (Maybe (Move, Zipper Peg), Zipper Peg)
    moveLeft (Zipper (l1:l2:ls) focus rs)
      | l2 == Empty && l1 == Peg  && focus == Peg =
          let newZipper = Zipper (Empty:Peg:ls) Empty rs
              focusIdx = getFocusIndex (Zipper (l1:l2:ls) focus rs)
              move = Move { from = focusIdx, over = focusIdx -1, to = focusIdx -2 }
          in Just (Just (move, newZipper), Zipper (l2:ls) l1 (focus:rs))
      | otherwise = Just (Nothing, Zipper (l2:ls) l1 (focus:rs))
    moveLeft _ = Nothing

    moveRight :: Zipper Peg -> Maybe (Maybe (Move, Zipper Peg), Zipper Peg)
    moveRight (Zipper ls focus (r1:r2:rs))
      | r2 == Empty && r1 == Peg && focus == Peg =
          let newZipper = Zipper ls Empty (Empty:Peg:rs)
              focusIdx = getFocusIndex (Zipper ls focus (r1:r2:rs))
              move = Move { from = focusIdx, over = focusIdx +1, to = focusIdx +2 }
          in Just (Just (move, newZipper), Zipper (focus:ls) r1 (r2:rs))
      | otherwise = Just (Nothing, Zipper (focus:ls) r1 (r2:rs))
    moveRight _ = Nothing

-- | Unfolds a move tree from a seed value.
--
-- === __Parameters__
-- * `f` - Function that generates the next tree node from a seed value.
-- * `x` - The seed value to start the tree generation.
--
-- === __Returns__
-- * A move tree generated from the seed value.
--
unfoldMoveTree :: (b -> Either a (a, [(Move, b)])) -> b -> MoveTree
unfoldMoveTree f x = case f x of
    Left a -> MoveLeaf a
    Right (a, bs) -> MoveNode a [ (move, unfoldMoveTree f b) | (move, b) <- bs ]

-- | Generates the game tree with moves for a given initial state.
-- The tree is generated using the `unfoldMoveTree` function, where each node represents a game state
-- and its children represent the next possible game states that can be reached by making a valid move.
--
-- === __Parameters__
-- * `initialState` - The initial game state represented as a `Zipper Peg`.
--
-- === __Returns__
-- * A `MoveTree` representing all possible game states with moves from the initial state.
--
makeGameTreeWithMoves :: Zipper Peg -> MoveTree
makeGameTreeWithMoves = unfoldMoveTree f
  where
    f :: Zipper Peg -> Either (Zipper Peg) (Zipper Peg, [(Move, Zipper Peg)])
    f zipper =
      let moves = makeMovesWithMoves zipper
      in if null moves
          then Left zipper
          else Right (zipper, moves)

-- | Retrieves a sequence of moves that leads to a winning state if one exists.
--
-- This function traverses the move tree to find a path to a winning state, collecting the moves along the way.
--
-- === __Parameters__
-- * `initialState` - The initial game state represented as a `Zipper Peg`.
--
-- === __Returns__
-- * `Just` a list of moves if a solution exists, `Nothing` otherwise.
--
-- === __Examples__
-- >>> getSolution (fromJust $ toZipper [Peg, Peg, Empty, Peg])
-- Just [Move {from = 3, over = 2, to = 1}, Move {from = 0, over = 1, to = 2}]
--
getSolution :: Zipper Peg -> Maybe [Move]
getSolution initialState = dfs (makeGameTreeWithMoves initialState)
  where
    dfs :: MoveTree -> Maybe [Move]
    dfs (MoveLeaf z)
      | isWinning (fromZipper z) = Just []
      | otherwise = Nothing
    dfs (MoveNode _ branches) = listToMaybe [ move : path | (move, mt) <- branches, Just path <- [dfs mt] ]

-- | Applies a move to the current game state.
--
-- === __Parameters__
-- * `zipper` - The current game state as a `Zipper Peg`.
-- * `move` - The move to apply.
--
-- === __Returns__
-- * The new game state after applying the move.
--
applyMove :: Zipper Peg -> Move -> Zipper Peg
applyMove zipper (Move fromIdx _ toIdx) =
  let pegs = fromZipper zipper
      updatedPegs = makeMove pegs (Move fromIdx undefined toIdx)
      newZipper = fromJust $ toZipperAtIndex updatedPegs toIdx
  in newZipper

-- | Applies a move to a list of pegs.
--
-- === __Parameters__
-- * `pegs` - The current list of pegs.
-- * `move` - The move to apply.
--
-- === __Returns__
-- * The new list of pegs after applying the move.
--
makeMove :: Pegs -> Move -> Pegs
makeMove pegs (Move from over to) =
  [ updatePeg i p | (i, p) <- zip [0..] pegs ]
  where
    updatePeg i p
      | i == from = Empty
      | i == over = Empty
      | i == to   = Peg
      | otherwise = p

-- | Converts a list into a zipper with the focus at a specified index.
--
-- === __Parameters__
-- * `pegs` - The list of pegs.
-- * `idx` - The index to set as focus.
--
-- === __Returns__
-- * `Just` zipper with focus at `idx` if valid, `Nothing` otherwise.
--
toZipperAtIndex :: [a] -> Int -> Maybe (Zipper a)
toZipperAtIndex pegs idx
  | idx < 0 || idx >= length pegs = Nothing
  | otherwise =
    let (left, focus:right) = splitAt idx pegs
    in Just (Zipper (reverse left) focus right)

-- | Tries a sequence of moves starting from the initial game state.
--
-- This function applies each move in the list to the game state and returns the final state.
--
-- === __Parameters__
-- * `initialState` - The initial game state as a `Zipper Peg`.
-- * `moves` - The list of moves to apply.
--
-- === __Returns__
-- * The final game state after applying all moves.
--
-- === __Examples__
-- >>> let initialState = fromJust $ toZipper [Peg, Peg, Empty, Peg]
-- >>> let Just moves = getSolution initialState
-- >>> fromZipper $ trySolution initialState moves
-- [Empty,Empty,Empty,Peg]
--
trySolution :: Zipper Peg -> [Move] -> Zipper Peg
trySolution initialState moves = foldl applyMove initialState moves


hasSolution = error "Implement, document, and test this function"
allSolutions = error "Implement, document, and test this function"
getSolution = error "Implement, document, and test this function"
trySolution = error "Implement, document, and test this function"
