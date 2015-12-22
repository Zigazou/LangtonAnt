{- |
Module      : Color
Description : Color
Copyright   : (c) Frédéric BISSON, 2015
License     : GPL-3
Maintainer  : zigazou@free.fr
Stability   : experimental
Portability : POSIX
-}
module Color where

{-|
Two colors: `Black` and `White`
-}
data Color = Black | White deriving (Eq, Ord, Enum, Show)

{-|
Invert a `Color`: `White` becomes `Black` and `Black` becomes `White`.
-}
invert :: Color -> Color
invert White = Black
invert Black = White
