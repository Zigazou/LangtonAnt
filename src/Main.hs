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

{-
Changing the direction module allows to change the original Langton Ant
behaviour. Direction4 is the original behaviour, exhibiting the highways.
Direction8 is an extension which does not know the highways.
-}
import qualified Direction4 as D4
import qualified Direction8 as D8

{-|
Move an `Ant` a number of time on a `Grid`, changing `Color` of cells.
`grid'` is forced strict via `seq` otherwise lazy evaluation would consume too
much memory for nothing.
-}
antWalk :: (Direction a) => Int -> Ant a -> Grid -> (Ant a, Grid)
antWalk 0 ant grid = (ant, grid)
antWalk n ant grid = grid' `seq` antWalk (pred n) ant' grid'
    where (ant', grid') = runState (iteration ant) grid

{-|
Repeat an ant walk on the same `Grid`, making it evolve.
-}
repeatAntWalk :: (Direction a) => Int -> Int -> Ant a -> Grid -> (Ant a, Grid)
repeatAntWalk 0 _ ant grid = (ant, grid)
repeatAntWalk rep count ant grid = repeatAntWalk (pred rep) count ant grid'
    where (_, grid') = antWalk count ant grid

repeatAntWalk' :: (Direction a) => Int -> Int -> Ant a -> Grid -> Grid
repeatAntWalk' rep count ant grid = grid'
    where (_, grid') = repeatAntWalk rep count ant grid

main :: IO ()
main = do
    [dirMod, scount, srep] <- getArgs

    let count = read scount
        rep = read srep

    let grid' = case dirMod of
                "4" -> repeatAntWalk' rep count (Ant (0, 0) D4.ToUp) newGrid 
                "8" -> repeatAntWalk' rep count (Ant (0, 0) D8.ToUp) newGrid
                _ -> error "Unknown direction module"

    print $ grid'^.bounds
    putStr $ unlines $ reverse $ toAscii (grid'^.bounds) grid'
