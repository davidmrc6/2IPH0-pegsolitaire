module Main (main) where

import PegSolitaire

main :: IO ()
main = do
        let pegs = stringToPegs "XXXX.XXXXX"
        putStrLn "The peg puzzle:"
        print pegs
        let answer = if hasSolution pegs then "a" else "no"
        putStrLn ("Has " ++ answer ++ " solution")
