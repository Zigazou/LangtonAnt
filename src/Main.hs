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

import Grid
import Ant
import Direction

loopState :: Int -> Ant -> Grid -> (Ant, Grid)
loopState 0 ant grid = (ant, grid)
loopState n ant grid = loopState (pred n) ant' grid'
    where (ant', grid') = runState (iteration ant) grid

loopLoopState :: Int -> Int -> Ant -> Grid -> (Ant, Grid)
loopLoopState 0 _ ant grid = (ant, grid)
loopLoopState rep count ant grid = loopLoopState (pred rep) count ant grid'
    where (_, grid') = loopState count ant grid

main :: IO ()
main = do
    [scount, srep] <- getArgs

    let ant = Ant (0, 0) ToUp
        count = read scount
        rep = read srep

    let (_, grid') = loopLoopState rep count ant newGrid

    putStr $ unlines $ reverse $ toAscii grid'
