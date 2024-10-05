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

----------------------------------

isWinning :: Pegs -> Bool
isWinning [] = False -- handle empty list case
isWinning [x] = isPeg x -- base case
isWinning (x:xs) = isPeg x `xor` isWinning xs

isPeg :: Peg -> Bool
isPeg x = case x of
    Peg -> True
    Empty -> False

generateStates = error "Implement, document, and test this function"
generateLinearStates = error "Implement, document, and test this function"

-- data Zipper a =

fromZipper = error "Implement, document, and test this function"
toZipper = error "Implement, document, and test this function"
tryRight = error "Implement, document, and test this function"
tryLeft = error "Implement, document, and test this function"
makeMoves = error "Implement, document, and test this function"
foldT = error "Implement, document, and test this function"
unfoldT = error "Implement, document, and test this function"
makeGameTree = error "Implement, document, and test this function"
hasSolution = error "Implement, document, and test this function"
allSolutions = error "Implement, document, and test this function"
getSolution = error "Implement, document, and test this function"
trySolution = error "Implement, document, and test this function"
