module Game.Coords where

import Prelude

import Data.Set (Set)
import Data.Set as Set
import Effect (Effect)


type Size =
  { rows :: Int
  , cols :: Int
  }


mkSize :: Int -> Int -> Size
mkSize rows cols =
  { rows: rows
  , cols: cols
  }


type Coord =
  { row :: Int
  , col :: Int
  }


mkCoord :: Int -> Int -> Coord
mkCoord row col =
  { row: row
  , col: col
  }


coords :: Size -> Array Coord
coords sz = []


neighbours :: Size -> Coord -> Array Coord
neighbours sz coord = []


validCoord :: Size -> Coord -> Boolean
validCoord sz coord = false


randomDistinctCoords :: Size -> Int -> Effect (Set Coord)
randomDistinctCoords sz count = pure Set.empty