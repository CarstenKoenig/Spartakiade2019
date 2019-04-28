module Game.Board where

import Prelude hiding (div)

import Algorithm.Closure (closure)
import Data.Array (foldl)
import Data.HashMap (HashMap)
import Data.HashMap as Map
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Game.Coords (Coord, Size, randomDistinctCoords, coords, neighbours)


type Board = 
  { map  :: HashMap Coord CellState
  , size :: Size
  }


type CellState =
  { state    :: State
  , cell     :: Cell
  }

data Cell
  = Mine
  | Free Int

derive instance eqCell :: Eq Cell


data State
  = Hidden
  | Revealed
  | Flagged
  | Marked


initialBoard :: Size -> Int -> Effect Board
initialBoard sz mineCount = do
  mineCoords <- (Set.toUnfoldable <$> randomDistinctCoords sz mineCount) :: Effect (Array Coord)
  pure $ foldl (flip insertMine) (emptyBoard sz) $ mineCoords


emptyBoard :: Size -> Board
emptyBoard sz =
  { map: emptyMap 
  , size: sz
  }
  where
  emptyMap = Map.fromArray $ map emptyCell $ coords sz
  emptyCell coord = Tuple coord { state: Hidden, cell: Free 0 }


insertMine :: Coord -> Board -> Board
insertMine coord board =
  board { map = updated }
  where
  updated = foldl (flip updateCount) withMine (neighbours board.size coord)
  withMine = Map.insert coord { state: Hidden, cell: Mine } board.map
  updateCount coord' = Map.update (Just <<< addOne) coord'
  addOne cellState =
    cellState { cell = addOneToCell cellState.cell }
  addOneToCell Mine = Mine
  addOneToCell (Free n) = Free (n+1)


getCellState :: Board -> Coord -> Maybe CellState
getCellState board coord =
  Map.lookup coord board.map


calculateRegionToReveal :: Board -> Coord -> Set Coord
calculateRegionToReveal board start =
  closure neigborsOfFree start
  where
    neigborsOfFree coord =
      case getCellState board coord of
        Just { cell: Free 0, state: Hidden } -> Set.fromFoldable $ neighbours board.size coord
        _ -> Set.empty