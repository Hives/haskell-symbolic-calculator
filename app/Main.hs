module Main where

import           SymbolicCalculator

main :: IO ()
main = (print . parse . tokenize) "x1 = -15 / (2 + x2)"
