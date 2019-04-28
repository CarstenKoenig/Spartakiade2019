module Game.Coords where

import Prelude

import Control.MonadPlus (guard)
import Data.Array as Array
import Data.Set (Set)
import Data.Set as Set
import Effect (Effect)
import Effect.Random (randomInt)


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
coords sz = do
  row <- Array.range 1 sz.rows
  col <- Array.range 1 sz.cols
  pure { row: row, col: col }


neighbours :: Size -> Coord -> Array Coord
neighbours sz coord = do
  row <- Array.range (coord.row - 1) (coord.row + 1)
  col <- Array.range (coord.col - 1) (coord.col + 1)
  let neighCoord = { row: row, col: col }
  guard (neighCoord /= coord && validCoord sz neighCoord)
  pure neighCoord


validCoord :: Size -> Coord -> Boolean
validCoord sz coord =
  1 <= coord.row && coord.row <= sz.rows &&
  1 <= coord.col && coord.col <= sz.cols


randomDistinctCoords :: Size -> Int -> Effect (Set Coord)
randomDistinctCoords sz count = go Set.empty
  where
  go set 
    | Set.size set >= count = pure set
    | otherwise = do
      coord <- randomCoord sz
      go $ Set.insert coord set 


randomCoord :: Size -> Effect Coord
randomCoord sz = do
  r <- randomInt 1 sz.rows
  c <- randomInt 1 sz.cols
  pure { row: r, col: c }