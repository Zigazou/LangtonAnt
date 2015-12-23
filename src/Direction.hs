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

class (Show a) => Direction a where
    {-|
    Turn right from a direction.
    -}
    turnRight :: Direction a => a -> a

    {-|
    Turn left from a direction.
    -}
    turnLeft :: Direction a => a -> a

    {-|
    Go forward given a `Direction`
    -}
    forward :: Direction a => a -> (Int, Int) -> (Int, Int)
