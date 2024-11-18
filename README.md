# Peg solitaire

A `Peg solitaire` game built in Haskell as a course assignment.

## Introduction 
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

## Functionality
This project consists of multiple functions for analysing and solving a Peg Solitare game.

## Testing
To run the tests, run the following command:
```bash
cabal test
```
