module Game.Board where

import Prelude hiding (div)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Game.Coords (Coord, Size)


type Board =  Unit

type CellState = Unit

data Cell
  = Mine
  | Free Int


data State
  = Hidden
  | Revealed
  | Flagged
  | Marked


initialBoard :: Size -> Int -> Effect Board
initialBoard sz mineCount = pure $ emptyBoard sz


emptyBoard :: Size -> Board
emptyBoard sz = unit


insertMine :: Coord -> Board -> Board
insertMine coord board = board


getCellState :: Board -> Coord -> Maybe CellState
getCellState board coord = Nothing