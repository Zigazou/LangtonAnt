{-# LANGUAGE TemplateHaskell #-}
{- |
Module      : Ant
Description : An Ant
Copyright   : (c) Frédéric BISSON, 2015
License     : GPL-3
Maintainer  : zigazou@free.fr
Stability   : experimental
Portability : POSIX

-}
module Ant where

import Control.Lens

import Direction

{-|
An `Ant` stands at a position, pointing to a direction.
-}
data Ant = Ant
    { _pos :: (Int, Int)
    , _dir :: Direction
    } deriving (Show)

makeLenses ''Ant

{-|
Change direction of an `Ant` and move forward.
-}
move :: (Direction -> Direction) -> Ant -> Ant
move turn (Ant p d) = Ant { _pos = forward (turn d) p
                          , _dir = turn d
                          }
