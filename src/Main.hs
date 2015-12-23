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

import System.Environment
import Control.Monad.State.Lazy
import Control.Lens

import Grid
import Ant
import Direction

{-|
Move an `Ant` a number of time on a `Grid`, changing `Color` of cells.
`grid'` is forced strict via `seq` otherwise lazy evaluation would consume too
much memory for nothing.
-}
antWalk :: Int -> Ant -> Grid -> (Ant, Grid)
antWalk 0 ant grid = (ant, grid)
antWalk n ant grid = grid' `seq` antWalk (pred n) ant' grid'
    where (ant', grid') = runState (iteration ant) grid

{-|
Repeat an ant walk on the same `Grid`, making it evolve.
-}
repeatAntWalk :: Int -> Int -> Ant -> Grid -> (Ant, Grid)
repeatAntWalk 0 _ ant grid = (ant, grid)
repeatAntWalk rep count ant grid = repeatAntWalk (pred rep) count ant grid'
    where (_, grid') = antWalk count ant grid

main :: IO ()
main = do
    [scount, srep] <- getArgs

    let ant = Ant (0, 0) ToUp
        count = read scount
        rep = read srep

    let (_, grid') = repeatAntWalk rep count ant newGrid

    putStr $ unlines $ reverse $ toAscii (grid'^.bounds) grid'
