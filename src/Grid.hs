{-# LANGUAGE TemplateHaskell #-}
{- |
Module      : Grid
Description : Grid
Copyright   : (c) Frédéric BISSON, 2015
License     : GPL-3
Maintainer  : zigazou@free.fr
Stability   : experimental
Portability : POSIX

-}
module Grid where

import Control.Lens
import Control.Monad.State.Lazy
import qualified Data.Map as Map
import Data.List
import Data.Ord

import Color
import Ant
import Direction

data Grid = Grid
    { _grid :: Map.Map (Int, Int) Color
    , _ant :: Ant
    , _bounds :: (Int, Int, Int, Int)
    } deriving (Show)

makeLenses ''Grid

newGrid :: Grid
newGrid = Grid
    { _grid = Map.singleton (0, 0) White
    , _ant = Ant { _pos = (0, 0), _dir = ToUp }
    , _bounds = (0, 0, 0, 0)
    }

updateBounds :: Monad m => (Int, Int) -> StateT (Int, Int, Int, Int) m ()
updateBounds (x, y) = do
    _1 %= min x
    _2 %= max x
    _3 %= min y
    _4 %= max y

getColorAt :: Monad m => (Int, Int) -> StateT Grid m Color
getColorAt position = do
    colorM <- use (grid.at position)

    case colorM of
         Nothing -> grid %= Map.insert position White >> return White
         Just c -> return c

iteration :: Monad m => StateT Grid m ()
iteration = do
    position <- use (ant.pos)
    color <- getColorAt position

    zoom bounds (updateBounds position)

    zoom ant $ case color of
                    White -> move turnRight
                    Black -> move turnLeft

    grid.at position._Just %= invert
