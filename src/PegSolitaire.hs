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
import Data.Bits (Bits(xor))

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

-- | Function that, given a Peg solitaire game, determines if
-- the current state is the winning state.
--
-- === __Examples__
-- TO DO
isWinning :: Pegs -> Bool
isWinning [] = False
isWinning [x] = isPeg x
isWinning (x:xs) = isPeg x `xor` isWinning xs

-- | Helper function that checks if a value is `Peg`. If it is, it returns
-- `True`. otherwise, it returns `False`.
isPeg :: Peg -> Bool
isPeg x = case x of
    Peg -> True
    Empty -> False

-- | Catamorphism factory for type `Tree`.
--
-- === __Examples__
-- TO DO
foldT :: (a -> b) -> (a -> [b] -> b) -> Tree a -> b
foldT fLeaf fNode = go
    where
        go (Leaf l) = fLeaf l
        go (Node n ts) = fNode n (map go ts)


generateStates = error "Implement, document, and test this function"
generateLinearStates = error "Implement, document, and test this function"

-- data Zipper a =

-- data Zipper a = ...
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

 
fromZipper = error "Implement, document, and test this function"
toZipper = error "Implement, document, and test this function"
tryRight = error "Implement, document, and test this function"
tryLeft = error "Implement, document, and test this function"
makeMoves = error "Implement, document, and test this function"
unfoldT = error "Implement, document, and test this function"
makeGameTree = error "Implement, document, and test this function"
hasSolution = error "Implement, document, and test this function"
allSolutions = error "Implement, document, and test this function"
getSolution = error "Implement, document, and test this function"
trySolution = error "Implement, document, and test this function"
