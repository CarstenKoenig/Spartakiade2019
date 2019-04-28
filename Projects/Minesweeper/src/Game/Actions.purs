module Game.Actions where

import Prelude hiding (div)

import Data.Array (foldl)
import Data.HashMap as Map
import Data.Maybe (Maybe(..))
import Game.Board (Board, State(..), calculateRegionToReveal)
import Game.Coords (Coord)
import Game.State (isRunning)


type Action = Board -> Board


reveal :: Coord -> Action
reveal coord board =
  foldl (flip $ updateCellState revealCell) board regionToReveal
  where
  regionToReveal = calculateRegionToReveal board coord
  revealCell Hidden = Revealed
  revealCell other  = other


cylceFlag :: Coord -> Action
cylceFlag = updateCellState cycleCell
  where
  cycleCell Hidden  = Flagged
  cycleCell Flagged = Marked
  cycleCell Marked  = Hidden
  cycleCell other   = other


updateCellState :: (State -> State) -> Coord -> Action
updateCellState update coord board
  | isRunning board =
    board { map = newMap }
    where
    newMap = Map.update (Just <<< updateState) coord board.map
    updateState cell = cell { state = update cell.state }
  | otherwise = board