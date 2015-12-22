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
module Grid (Grid, newGrid, iteration, toAscii) where

import Control.Lens
import Control.Monad.State.Lazy
import qualified Data.Map as Map

import Color (Color(Black, White), invert)
import Ant (Ant, pos, move)
import Direction (turnLeft, turnRight)

type Boundaries = (Int, Int, Int, Int)
type Position = (Int, Int)

{-|
A `Grid` contains a serie of cells, an `Ant` and keeps track of boundaries.
-}
data Grid = Grid
    { _grid :: Map.Map Position Color
    , _bounds :: Boundaries
    } deriving (Show)

makeLenses ''Grid

{-|
Generate a new `Grid`, placing the `Ant` at its center pointing upward.
-}
newGrid :: Grid
newGrid = Grid (Map.singleton (0, 0) White) (0, 0, 0, 0)

{-|
Given a position, expands boundaries to fit it in.
-}
updateBounds :: Position -> Boundaries -> Boundaries
updateBounds (x, y) (minx, maxx, miny, maxy) =
    (min x minx, max x maxx, min y miny, max y maxy)

{-|
Get the color at a position in a `Grid`. If the position does not yet exist,
it is created `White` and the boundaries are updated. It is needed because the
`Grid` keeps only track of cells that have been visited by the `Ant`.
-}
getColorAt :: Monad m => Position -> StateT Grid m Color
getColorAt position = do
    colorM <- use (grid.at position)

    case colorM of
         Nothing -> do
            grid %= Map.insert position White
            bounds %= updateBounds position
            return White
         Just c -> return c

{-|
An iteration makes the `Ant` move according to the Langton rules:

- if the `Ant` is on a `Black` cell, it turns left and move,
- if the `Ant` is on a `White` cell, it turns right and move.

The cell being leaved sees its color inverted (black becomes white and white
becomes black).
-}
iteration :: Monad m => Ant -> StateT Grid m Ant
iteration ant = do
    let position = ant^.pos
    color <- getColorAt position
    grid.at position._Just %= invert

    return $ case color of
                  White -> move turnLeft ant
                  Black -> move turnRight ant

{-|
Generates a list of `String` representing the `Grid` with Ascii characters
(more precisely UTF-8 characters).
-}
toAscii :: Grid -> [String]
toAscii g = [ [ colorAt (x, y) | x <- [minx .. maxx] ] | y <- [miny .. maxy] ]
    where (minx, maxx, miny, maxy) = g^.bounds
          colorAt xy = case g^.grid.at xy of
                            Nothing -> ' '
                            Just White -> '⋅'
                            Just Black -> '█'

