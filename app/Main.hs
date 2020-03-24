module Main where

import           SymbolicCalculator

main :: IO ()
main = print $ tokenize " (666 + 18067) / R2D2 = x"
