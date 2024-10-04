# Assignment 2—Peg solitaire

## Introduction

In this assignment, you will write a program to see if a peg solitaire puzzle has a solution whilst using a `Zipper` data structure. The zipper data structure allows data to have a focus element with constant-time access.

The one-dimensional peg solitaire puzzle has a sequence of positions. Each position is either empty or has a peg. We call this sequence the _game state_. For instance:
```
X X X X . X X X X
```
`X` indicates a peg, and `.` indicates an empty position. A peg can jump (either to the left or right) over another peg towards an empty position and removes the peg it jumped over. Thus, we can make the third peg jump over the fourth in the above example and get the following game state.
```
X X . . X X X X X
```
This is one move, and to solve the puzzle, we make moves until no valid moves are left. The player wins if one peg remains in the end state. Not all starting configurations have a winning solution. For instance,
```
X . . . . . . . X
```
is not winning since no moves are possible, and two pegs are left.

In this assignment, we want you to analyse a starting position and decide if it is winning or not by considering all possible moves.

## Assignment

This assignment consists of ten programming exercises.

In the ten programming exercises, you must explicitly
add a type declaration to every function you define in Haskell and add Haddock documentation. As part of
these ten programming exercises, you also have to write tests in
[`test/Spec.hs`](test/Spec.hs) for each function we ask of you to define.

### Split over two weeks

### Grading

We grade you on exercises 1 through 4 based on your submission in the first week.
We grade you on exercises 5 through 9 based on your submission in the second week.
Exercise 10 is a bonus exercise, which you do to challenge yourself.


### Getting started

1. Clone this repository. It contains a Cabal project. Do not *manually* add
    files to or remove files from this project, nor change any file names. All
    you have to do is change the contents of
    [`src/PegSolitaire.hs`](src/PegSolitaire.hs), and
    [`test/Spec.hs`](test/Spec.hs).

2. Verify that the project works by running `cabal test`. All tests should pass.

3. Do the exercises in this README. Invariant: All tests pass.

    "All tests pass" means that your project should build. **If your project
    does not build, you fail this assignment**. If you are unable to get
    something to work, comment out the problematic parts to keep the project
    working.

4. In completing this assignment, you deliver two files:

    * The Haskell file [`src/PegSolitaire.hs`](src/PegSolitaire)
    * The Haskell file [`test/Spec.hs`](test/Spec.hs)

    Do not forget to have your names, student numbers, and date visible and at
    the top of all files you edit.

## Exercises

### Program the core functionality in module `PegSolitaire`

#### Exercise 1: `isWinning`
Define function `isWinning` that, given a Peg solitaire game, determines if the current state is a winning state. That is, there is only one Peg left in the game.

#### Exercise 2: `foldT`
Define function `foldT`, which is the catamorphism factory for type `Tree`, to help transform a `Tree` into some other value. Compare this to `foldr` and `foldTree` (from [`Data.Tree`](https://hackage.haskell.org/package/containers-0.6.6/docs/Data-Tree.html)).

#### Exercise 3: `Zipper`, `toZipper`, `fromZipper`, `tryRight`, `tryLeft`
We can store the normal one-dimensional game state in a list. However, we can only access the first element in a list in constant time. Another way to do this is to introduce a `Zipper` structure, we can do this for most data structures, but in this assignment, we focus on the zipper of a list. In a Zipper, you 'walk' through the data structure present. You store the current value under focus, the remainder of the data structure, and a history of how you walked through the structure. For a list, you go from left to right through the list. So the focus is the current value, the remainder of the data structure is the list of all values to the right of the focus, and the history is all the values left of the focus. When you store the history in reverse order, you can prepend the focus when moving to the right.

For instance the list `[1,2,3,4,5]`, we can have `3` as focus. Then, `[4,5]` is the remainder and `[2,1]` is the history. So when we go one place to the right, we get `4` as new focus, `[5]` as a new remainder and `[3,2,1]` as new history.

Define the data structure `Zipper a = ...`, which stores a list of type `[a]` as a zipper structure. Then, define helper functions `toZipper` and `fromZipper` that turn a list into a zipper structure and visa versa. Also, define functions `tryRight` and `tryLeft` that change the focus of a zipper one position to the right or left. They should return a `Maybe` type. If the zipper cannot go further right or left, it should return `Nothing`.

*Hint:* It might help to make your `Zipper` an instance of `Show` to explore
zippers in GHCi. To make reading zippers more convenient, reverse the 
reversed part left of the focus. I.e., you could turn the example into the
string `[1,2](3)[4,5]`.


#### Exercise 4: `generateLinearStates` and `generateStates` via `unfoldr`
Given integer `n`, define the following functions using [`unfoldr`](https://hackage.haskell.org/package/base-4.17.2.1/docs/Data-List.html#v:unfoldr) (from `Data.List`).

**`generateLinearStates`**: This function should give all possible states with `n-1` positions filled with pegs and one empty position. E.g. `generateLinearStates 2` should give the elements `X .` and `. X`.

**`generateStates`**: This function gives a list of all possible states up to size `n`. E.g. `generateLinearStates 2` should give the elements `.`, `X`, `. .`, `X .`, `. X` and `X X`.

*Hint:* To define `generateStates` you might want to use some sort of tupling.

*Note:* You can use these functions in the remainder of the Assignment to have a broad test set.

#### Exercise 5: `makeMoves` via `unfoldr`
Define function `makeMoves`, which, given a Zipper for the current game state, generates a (normal) list of all game states that can be reached by making one valid move. The returned gamestates should be zippers.

Hint: You should use the function `unfoldr` twice. Once for the left side (history) of the zipper, and once for the right side (the remainder). The function [`catMaybes`](https://hackage.haskell.org/package/base-4.17.2.1/docs/Data-Maybe.html#v:catMaybes) (from Data.Maybe) might come in handy.

#### Exercise 6: `unfoldT`
Define function `unfoldT`, which is the anamorphism factory for type `Tree`, to help create a `Tree` from a seed. Compare this to `unfoldr` (from `Data.List`) and `unfoldTree` (from `Data.Tree`).

#### Exercise 7: `makeGameTree`
Use the `Tree` data type we defined to explore all states that can be reached from an initial state. The node should store the current state and all the next states it can reach by making a valid move. Use a leaf to store a game state that cannot take any more moves. Define function `makeGameTree` as instance of `unfoldT` to produce the game tree for a given initial state given as `Zipper`.

#### Exercise 8: `hasSolution` as hylo
Define function `hasSolution` that determines if a starting game state has a solution as a hylomorphism, that is, as a catamorphism (defined via `foldT`) after an anamorphism (defined via `unfoldT`). I.e., fold over the output of an application to `makeGameTree`.

#### Exercise 9 `allSolutions` as hylo
Define function `allSolutions` that gives all possible winning end states as a hylomorphism.

#### [Bonus] Exercise 10: `getSolution`, `trySolution`
Define function `getSolution` that gives back a sequence of moves that a player can take to get a solution if a starting game state has a solution. Also define function `trySolution`, that given the outcome of `getSolution` can try the moves and gives the game state after taking a sequence of moves.

*Note*: We do not specify how this sequence of moves should look like. Define a suitable data type yourself that fits here.

*Important*: Do not break previously defined functions when working on this exercise.
