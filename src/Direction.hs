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

{-|
Directions on a cartesian plane (up, down, left, right). These are absolute
direction.
-}
data Direction = ToUp | ToRight | ToDown | ToLeft deriving (Eq, Ord, Enum, Show)

{-|
Turn right from a direction.
-}
turnRight :: Direction -> Direction
turnRight ToLeft = ToUp
turnRight d = succ d

{-|
Turn left from a direction.
-}
turnLeft :: Direction -> Direction
turnLeft ToUp = ToLeft
turnLeft d = pred d

{-|
Go forward given a `Direction`
-}
forward :: Direction -> (Int, Int) -> (Int, Int)
forward ToUp    (x, y) = (x, y + 1)
forward ToRight (x, y) = (x + 1, y)
forward ToDown  (x, y) = (x, y - 1)
forward ToLeft  (x, y) = (x - 1, y)
