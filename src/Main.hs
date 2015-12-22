{- |
Module      : Main
Description : Langton Ant
Copyright   : (c) Frédéric BISSON, 2015
License     : GPL-3
Maintainer  : zigazou@free.fr
Stability   : experimental
Portability : POSIX

-}
module Main where

import Control.Monad
import Control.Monad.State.Lazy

import Grid
import Ant

main :: IO ()
main = do
    let grid = newGrid
    let grid' = execState (replicateM 10 iteration) grid
    print grid'