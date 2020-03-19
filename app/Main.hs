module Main where

import SymbolicCalculator

main :: IO ()
main = print $ tokenize " 1 + 4 / x "
