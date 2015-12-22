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
import Control.Monad
import Control.Monad.State.Lazy

import Grid

main :: IO ()
main = do
    (count:_) <- getArgs

    let grid = execState (replicateM (read count) iteration) newGrid

    putStr $ unlines $ reverse $ toAscii grid
