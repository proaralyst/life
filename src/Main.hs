module Main where

import Life (loop, init)

import Control.Monad (void)

main :: IO ()
main = void $ loop Life.init
