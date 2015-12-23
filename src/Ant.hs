{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Rank2Types #-}
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
data Ant a = Ant
    { _pos :: (Int, Int)
    , _dir :: a
    } deriving (Show)

makeLenses ''Ant

{-|
Change direction of an `Ant` and move forward.
-}
move :: Direction a => (a -> a) -> Ant a -> Ant a
move turn (Ant p d) = Ant { _pos = forward (turn d) p
                          , _dir = turn d
                          }
