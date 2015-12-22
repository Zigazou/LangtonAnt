{- |
Module      : Direction
Description : Direction
Copyright   : (c) Frédéric BISSON, 2015
License     : GPL-3
Maintainer  : zigazou@free.fr
Stability   : experimental
Portability : POSIX

-}
module Direction where

import Control.Lens

data Direction = ToUp
               | ToRight
               | ToDown
               | ToLeft
               deriving (Eq, Ord, Enum, Show)

turnRight :: Direction -> Direction
turnRight ToUp    = ToRight
turnRight ToRight = ToDown
turnRight ToDown  = ToLeft
turnRight ToLeft  = ToUp

turnLeft :: Direction -> Direction
turnLeft ToUp    = ToLeft
turnLeft ToRight = ToUp
turnLeft ToDown  = ToRight
turnLeft ToLeft  = ToDown

forward :: Direction -> (Int, Int) -> (Int, Int)
forward ToUp c    = c & _2 %~ (+) 1
forward ToRight c = c & _1 %~ (+) 1
forward ToDown c  = c & _2 %~ (-) 1
forward ToLeft c  = c & _1 %~ (-) 1
